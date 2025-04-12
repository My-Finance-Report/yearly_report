import asyncio
from dataclasses import replace

from app.async_pipelines.uploaded_file_pipeline.categorizer import (
    categorize_extracted_transactions,
    update_filejob_with_nickname,
)
from app.async_pipelines.uploaded_file_pipeline.local_types import (
    CategorizedTransaction,
    InProcessJob,
    PartialTransaction,
    Recategorization,
    TransactionsWrapper,
)
from app.func_utils import pipe
from app.models import (
    AuditLog,
    Category,
    CategoryId,
    PlaidTransactionId,
    ProcessingState,
    Transaction,
    TransactionId,
    TransactionSource,
    UploadConfiguration,
)
from app.worker.status import log_completed, status_update_monad


def apply_existing_transactions(in_process: InProcessJob) -> InProcessJob:
    assert in_process.config, "must have config"
    query = in_process.session.query(Transaction).filter(
        Transaction.transaction_source_id == in_process.config.transaction_source_id,
        Transaction.user_id == in_process.user.id,
        ~Transaction.archived,
    )
    if in_process.file:
        query = query.filter(
            Transaction.uploaded_pdf_id == in_process.file.id,
        )
    return replace(
        in_process,
        transactions=TransactionsWrapper(
            transactions=[
                PartialTransaction(
                    partialTransactionId=row.id,
                    partialPlaidTransactionId=row.external_id,
                    partialTransactionAmount=row.amount,
                    partialTransactionDescription=row.description,
                    partialTransactionDateOfTransaction=row.date_of_transaction.strftime(
                        "%m/%d/%Y"
                    ),
                    partialTransactionKind=row.kind.value,
                )
                for row in query
            ]
        ),
    )


def apply_previous_recategorizations(in_process: InProcessJob) -> InProcessJob:
    assert in_process.transaction_source, "must have transaction source"
    assert in_process.categories, "must have categories"
    query = (
        in_process.session.query(AuditLog, Transaction)
        .join(Transaction, Transaction.id == AuditLog.transaction_id)
        .filter(
            Transaction.transaction_source_id == in_process.transaction_source.id,
            Transaction.user_id == in_process.user.id,
            ~Transaction.archived,
            AuditLog.apply_to_future,
        )
        .all()
    )

    category_lookup = {cat.id: cat.name for cat in in_process.categories}

    recats: list[Recategorization] = []
    auditlog: AuditLog
    transaction: Transaction
    for auditlog, transaction in query:
        if auditlog.change.new_kind and auditlog.change.old_kind:
            recats.append(
                Recategorization(
                    description=transaction.description,
                    previous_category=auditlog.change.old_kind,
                    overrided_category=auditlog.change.new_kind,
                )
            )
        if auditlog.change.new_category and auditlog.change.old_category:
            recats.append(
                Recategorization(
                    description=transaction.description,
                    previous_category=category_lookup[auditlog.change.old_category],
                    overrided_category=category_lookup[auditlog.change.new_category],
                )
            )

    return replace(
        in_process,
        previous_recategorizations=recats or None,
    )


def apply_upload_config_no_create(process: InProcessJob) -> InProcessJob:
    assert process.job, "must have job"
    assert process.job.config_id, "MUST have config id"
    config: UploadConfiguration | None = None
    if process.job.config_id:
        config = (
            process.session.query(UploadConfiguration)
            .filter(UploadConfiguration.id == process.job.config_id)
            .first()
        )

    if not config:
        assert process.file, "must have file"
        query = (
            process.session.query(UploadConfiguration)
            .filter(UploadConfiguration.user_id == process.user.id)
            .all()
        )
        reg_lookup = {u.id: u.filename_regex.lower() for u in query}
        lookup = {u.id: u for u in query}

        raw_content = f"{process.file.filename} {process.file.raw_content}".lower()
        for id, filename_regex in reg_lookup.items():
            if filename_regex in raw_content:
                config = lookup[id]

    assert config is not None, "must have, no create"

    transaction_source = (
        process.session.query(TransactionSource)
        .filter(TransactionSource.id == config.transaction_source_id)
        .one()
    )
    categories = (
        process.session.query(Category)
        .filter(Category.source_id == config.transaction_source_id)
        .all()
    )

    return replace(
        process,
        config=config,
        transaction_source=transaction_source,
        categories=categories,
    )


def insert_recategorized_transactions(in_process: InProcessJob) -> InProcessJob:
    assert in_process.transaction_source, "must have transaction source"
    assert in_process.categorized_transactions, "must have categorized transactions"
    assert all(
        t.partialTransactionId is not None for t in in_process.categorized_transactions
    ), "must have"
    assert in_process.categories, "must have"

    category_lookup: dict[str, CategoryId] = {
        cat.name: cat.id for cat in in_process.categories
    }
    transaction_lookup: dict[
        TransactionId | PlaidTransactionId, CategorizedTransaction
    ] = {
        t.partialTransactionId: t
        for t in in_process.categorized_transactions
        if t.partialTransactionId is not None
    }

    existing_transactions = (
        in_process.session.query(Transaction)
        .filter(
            Transaction.id.in_(transaction_lookup.keys()),
            Transaction.user_id == in_process.user.id,
        )
        .all()
    )

    assert len(existing_transactions) == len(transaction_lookup), "must have"

    for transaction in existing_transactions:
        transaction.category_id = category_lookup[
            transaction_lookup[transaction.id].category
        ]

    in_process.session.commit()

    return in_process


async def recategorize_file_pipeline(in_process_files: list[InProcessJob]) -> None:
    in_process_with_config = [
        apply_upload_config_no_create(in_process) for in_process in in_process_files
    ]
    await async_batch_reprocess_files_with_config(in_process_with_config)


async def reprocess_file_async(in_process: InProcessJob) -> None:

    def pipeline_runner() -> None:
        return pipe(
            in_process,
            lambda x: status_update_monad(x, status=ProcessingState.preparing_for_parse, additional_info="Building the configuration"),
            apply_upload_config_no_create,
            lambda x: status_update_monad(x, status=ProcessingState.preparing_for_parse, additional_info="Applying previous recategorizations"),
            apply_previous_recategorizations,
            lambda x: status_update_monad(x, status=ProcessingState.preparing_for_parse, additional_info="Applying existing transactions"),
            apply_existing_transactions,
            lambda x: status_update_monad(x, status=ProcessingState.categorizing_transactions, additional_info="Categorizing batches of transactions"),
            categorize_extracted_transactions,
            lambda x: status_update_monad(x, status=ProcessingState.categorizing_transactions, additional_info="Updating file nickname"),
            update_filejob_with_nickname,
            lambda x: status_update_monad(x, status=ProcessingState.categorizing_transactions, additional_info="Writing transactions to the database"),
            insert_recategorized_transactions,
            final= lambda x: log_completed(x, additional_info="Completed recategorization"),
        )

    return await asyncio.to_thread(pipeline_runner)


async def reprocess_files_with_config_async(files: list[InProcessJob]) -> None:
    """Process multiple files with pre-determined configuration in parallel."""
    await asyncio.gather(*[reprocess_file_async(file) for file in files])


async def async_batch_reprocess_files_with_config(files: list[InProcessJob]) -> None:
    """Process multiple files with pre-determined configuration."""
    await reprocess_files_with_config_async(files)
