import logging
import re
from dataclasses import replace

from app.async_pipelines.uploaded_file_pipeline.configuration_creator import (
    create_configurations,
)
from app.async_pipelines.uploaded_file_pipeline.local_types import (
    InProcessJob,
    TransactionsWrapper,
)
from app.models import (
    Category,
    JobStatus,
    Transaction,
    TransactionSource,
    UploadConfiguration,
    WorkerJob,
)
from app.open_ai_utils import ChatMessage, make_chat_request

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


def generate_transactions_prompt(process: InProcessJob) -> list[str]:
    """Generate the AI prompt(s) for parsing transactions."""

    assert process.transaction_source, "must have"
    assert process.file, "must have"

    base_prompt = f"""
        Parse the following PDF content into a JSON array of transactions.
        Structure the dates as MM/DD/YYYY.
        Each transaction should have the fields: 'transactionDate', 'description', 'kind', and 'amount'.

        For banks, 'withdrawal' and 'deposit' should be clear.
        For credit and debit cards, a purchase is a withdrawal, and a payment on the card is a deposit.

        This file is for {process.transaction_source.name}.

        {process.file.filename}
    """

    max_tokens = 9000
    reserved_for_prompt = 3000  # assume this base_prompt takes up ~3000 tokens
    max_chunk_tokens = max_tokens - reserved_for_prompt
    max_chunk_chars = max_chunk_tokens * 4  # rough estimate of char limit

    content = process.file.raw_content

    # Split into lines and batch based on char length
    lines = content.splitlines()
    prompts: list[str] = []
    current_chunk: list[str] = []

    current_len = 0
    for line in lines:
        if current_len + len(line) > max_chunk_chars:
            chunk_text = "\n".join(current_chunk)
            prompts.append(base_prompt + chunk_text)
            current_chunk = [line]
            current_len = len(line)
        else:
            current_chunk.append(line)
            current_len += len(line)

    # Append last chunk
    if current_chunk:
        chunk_text = "\n".join(current_chunk)
        prompts.append(base_prompt + chunk_text)

    return prompts


def already_processed(process: InProcessJob) -> bool:
    assert process.job, "must have"

    val = (
        process.session.query(WorkerJob)
        .filter(
            WorkerJob.id == process.job.id,
            WorkerJob.status == JobStatus.completed,
        )
        .one_or_none()
    )

    logger.info(f"Already processed: {val}")
    return bool(val)


def apply_upload_config(process: InProcessJob) -> InProcessJob:
    assert process.job, "must have"
    assert process.file, "must have"

    logger.info(f"Applying upload configuration for file: {process.file.filename}")

    config: None | UploadConfiguration = None
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


def archive_transactions_if_necessary(process: InProcessJob) -> InProcessJob:
    """Remove existing transactions if the file has been processed before."""
    assert process.file, "must have"

    logger.info(f"Removing previous transactions for file: {process.file.filename}")
    query = process.session.query(Transaction).filter(
        Transaction.uploaded_pdf_id == process.file.id,
        Transaction.user_id == process.user.id,
    )

    logger.info(f"Removing previous transactions: {query.count()}")

    query.delete()

    process.session.commit()

    return process


def request_llm_parse_of_transactions(process: InProcessJob) -> InProcessJob:
    """Request AI to parse transactions from the extracted text."""
    if process.config is None:
        raise ValueError(
            "Upload configuration is required before parsing transactions."
        )

    prompts = generate_transactions_prompt(process)
    all_parsed_transactions = []

    for prompt in prompts:
        parsed_transactions = make_chat_request(
            TransactionsWrapper, [ChatMessage(role="user", content=prompt)]
        )
        if parsed_transactions:
            all_parsed_transactions.extend(parsed_transactions.transactions)

    return replace(process, transactions=TransactionsWrapper(transactions=all_parsed_transactions))
