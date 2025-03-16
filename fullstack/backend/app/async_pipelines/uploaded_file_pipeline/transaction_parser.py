import logging
import re
from dataclasses import replace

from app.async_pipelines.uploaded_file_pipeline.configuration_creator import (
    create_configurations,
)
from app.async_pipelines.uploaded_file_pipeline.local_types import (
    InProcessFile,
    TransactionsWrapper,
)
from app.models import (
    Category,
    JobStatus,
    ProcessFileJob,
    Transaction,
    TransactionSource,
    UploadConfiguration,
)
from app.open_ai_utils import ChatMessage, make_chat_request

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


def generate_transactions_prompt(process: InProcessFile) -> str:
    """Generate the AI prompt for parsing transactions."""

    assert process.transaction_source, "must have transaction source by here"

    return f"""
    Parse the following PDF content into a JSON array of transactions.
    Structure the dates as MM/DD/YYYY.
    Each transaction should have the fields: 'transactionDate', 'description', 'kind', and 'amount'.

    For banks, 'withdrawal' and 'deposit' should be clear.
    For credit and debit cards, a purchase is a withdrawal, and a payment on the card is a deposit.

    This file is for {process.transaction_source.name}.

    {process.file.filename}
    {process.file.raw_content}
    """


def already_processed(process: InProcessFile) -> bool:
    val = (
        process.session.query(ProcessFileJob)
        .filter(
            ProcessFileJob.id == process.job.id,
            ProcessFileJob.status == JobStatus.completed,
        )
        .one_or_none()
    )

    logger.info(f"Already processed: {val}")
    return bool(val)


def apply_upload_config(process: InProcessFile) -> InProcessFile:
    config: None | UploadConfiguration = None
    logger.info(f"Applying upload configuration for file: {process.file.filename}")

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
        reg_lookup = {u.id: u.filename_regex for u in query}
        lookup = {u.id: u for u in query}

        raw_content = f"{process.file.filename}"
        for id, filename_regex in reg_lookup.items():
            if re.search(filename_regex, raw_content, re.IGNORECASE):
                config = lookup[id]
                break

    if not config:
        config = create_configurations(process)

    assert config, "Should have generated a config by now"

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


def archive_transactions_if_necessary(process: InProcessFile) -> InProcessFile:
    """Remove existing transactions if the file has been processed before."""

    logger.info(f"Removing previous transactions for file: {process.file.filename}")
    query = process.session.query(Transaction).filter(
        Transaction.uploaded_pdf_id == process.file.id,
        Transaction.user_id == process.user.id,
    )

    logger.info(f"Removing previous transactions: {query.count()}")

    query.delete()

    process.session.commit()

    return process


def request_llm_parse_of_transactions(process: InProcessFile) -> InProcessFile:
    """Request AI to parse transactions from the extracted text."""
    if process.config is None:
        raise ValueError(
            "Upload configuration is required before parsing transactions."
        )

    prompt = generate_transactions_prompt(process)

    parsed_transactions = make_chat_request(
        TransactionsWrapper, [ChatMessage(role="user", content=prompt)]
    )

    return replace(process, transactions=parsed_transactions)
