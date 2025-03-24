import asyncio
from dataclasses import replace

from app.async_pipelines.uploaded_file_pipeline.categorizer import (
    categorize_extracted_transactions,
    update_filejob_with_nickname,
)
from app.async_pipelines.uploaded_file_pipeline.local_types import (
    CategorizedTransaction,
    InProcessFile,
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
    Transaction,
    TransactionId,
    TransactionSource,
    UploadConfiguration,
)


def apply_existing_transactions(in_process: InProcessFile) -> InProcessFile:
    assert in_process.config, "must have"
    assert in_process.file, "must have"
    query = in_process.session.query(Transaction).filter(
        Transaction.transaction_source_id == in_process.config.transaction_source_id,
        Transaction.user_id == in_process.user.id,
        Transaction.uploaded_pdf_id == in_process.file.id,
        ~Transaction.archived,
    )
    return replace(
        in_process,
        transactions=TransactionsWrapper(
            transactions=[
                PartialTransaction(
                    partialTransactionId=row.id,
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


def apply_previous_recategorizations(in_process: InProcessFile) -> InProcessFile:
    assert in_process.transaction_source, "must have"
    assert in_process.file, "must have"
    assert in_process.categories, "must have"
    query = (
        in_process.session.query(AuditLog, Transaction)
        .join(Transaction, Transaction.id == AuditLog.transaction_id)
        .filter(
            Transaction.transaction_source_id == in_process.transaction_source.id,
            Transaction.user_id == in_process.user.id,
            Transaction.uploaded_pdf_id == in_process.file.id,
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


def apply_upload_config_no_create(process: InProcessFile) -> InProcessFile:
    assert process.job, "must have"
    assert process.job.config_id, "MUST have"
    assert process.file, "must have"
    config: UploadConfiguration | None = None
    if process.job.config_id:
        config = (
            process.session.query(UploadConfiguration)
            .filter(UploadConfiguration.id == process.job.config_id)
            .first()
        )

    if not config:
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


def insert_recategorized_transactions(in_process: InProcessFile) -> None:
    assert in_process.transaction_source, "must have"
    assert in_process.categorized_transactions, "must have"
    assert in_process.file, "must have"
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
            Transaction.uploaded_pdf_id == in_process.file.id,
        )
        .all()
    )

    assert len(existing_transactions) == len(transaction_lookup), "must have"

    for transaction in existing_transactions:
        transaction.category_id = category_lookup[
            transaction_lookup[transaction.id].category
        ]

    in_process.session.commit()


async def recategorize_file_pipeline(in_process_files: list[InProcessFile]) -> None:
    in_process_with_config = [
        apply_upload_config_no_create(in_process) for in_process in in_process_files
    ]
    print(f"batch processing {len(in_process_with_config)}")
    await async_batch_reprocess_files_with_config(in_process_with_config)


async def reprocess_file_async(in_process: InProcessFile) -> None:
    def blah() -> None:
        return pipe(
            in_process,
            apply_upload_config_no_create,
            apply_previous_recategorizations,
            apply_existing_transactions,
            categorize_extracted_transactions,
            update_filejob_with_nickname,
            final=insert_recategorized_transactions,
        )

    return await asyncio.to_thread(blah)


async def reprocess_files_with_config_async(files: list[InProcessFile]) -> None:
    """Process multiple files with pre-determined configuration in parallel."""
    await asyncio.gather(*[reprocess_file_async(file) for file in files])


async def async_batch_reprocess_files_with_config(files: list[InProcessFile]) -> None:
    """Process multiple files with pre-determined configuration."""
    await reprocess_files_with_config_async(files)
