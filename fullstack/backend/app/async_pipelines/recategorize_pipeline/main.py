from dataclasses import replace

from app.async_pipelines.uploaded_file_pipeline.categorizer import (
    categorize_extracted_transactions,
    insert_categorized_transactions,
)
from app.async_pipelines.uploaded_file_pipeline.local_types import (
    InProcessFile,
    PartialTransaction,
    TransactionsWrapper,
)
from app.async_pipelines.uploaded_file_pipeline.transaction_parser import (
    archive_transactions_if_necessary,
)
from app.func_utils import pipe
from app.models import Category, Transaction, TransactionSource, UploadConfiguration


def apply_existing_transactions(in_process: InProcessFile) -> InProcessFile:
    assert in_process.config, "must have"
    query = in_process.session.query(Transaction).filter(
        Transaction.transaction_source_id == in_process.config.transaction_source_id,
        Transaction.user_id == in_process.user.id,
        ~Transaction.archived,
    )
    return replace(
        in_process,
        transactions=TransactionsWrapper(
            transactions=[
                PartialTransaction(
                    partialTransactionAmount=row.amount,
                    partialTransactionDescription=row.description,
                    partialTransactionDateOfTransaction=row.date_of_transaction.strftime(
                        "%d/%m/%Y"
                    ),
                    partialTransactionKind=row.kind.value,
                )
                for row in query
            ]
        ),
    )


def apply_upload_config_no_create(process: InProcessFile) -> InProcessFile:
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


def recategorize_pipeline(in_process: InProcessFile) -> None:
    pipe(
        in_process,
        apply_upload_config_no_create,
        apply_existing_transactions,
        archive_transactions_if_necessary,
        categorize_extracted_transactions,
        final=insert_categorized_transactions,
    )
