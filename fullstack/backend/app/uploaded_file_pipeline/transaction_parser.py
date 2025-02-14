from dataclasses import  replace
import logging
import subprocess

from fullstack.backend.app.open_ai_utils import ChatMessage, Prompt, make_chat_request
from fullstack.backend.app.uploaded_file_pipeline.categorizer import PdfParseException
from fullstack.backend.app.uploaded_file_pipeline.configuration_creator import create_configurations
from fullstack.backend.app.uploaded_file_pipeline.local_types import InProcessFile, TransactionsWrapper
from fullstack.backend.func_utils import make_batches, safe_pipe
from sqlalchemy import select, text



from app.models import JobStatus, ProcessFileJob, TransactionSource, UploadedPdf, User, UploadConfiguration

# Logging setup
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

def extract_text_from_pdf(pdf_path: str) -> str:
    """Extract text from a PDF file using `pdftotext`."""
    command = "pdftotext"
    args = ["-layout", pdf_path, "-"]
    
    try:
        result = subprocess.run(
            [command] + args,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            check=True,
            text=True  
        )
        return result.stdout.strip()
    except subprocess.CalledProcessError as e:
        raise PdfParseException(f"Failed to extract text from PDF: {e.stderr}") from e



def generate_transactions_prompt(process: InProcessFile) -> Prompt:
    """Generate the AI prompt for parsing transactions."""

    assert process.transaction_source, "must have transaction source by here"

    return [ChatMessage(role="user", message=f"""
    Parse the following PDF content into a JSON array of transactions.
    Structure the dates as MM/DD/YYYY.
    Each transaction should have the fields: 'transactionDate', 'description', 'kind', and 'amount'.

    For banks, 'withdrawal' and 'deposit' should be clear.
    For credit and debit cards, a purchase is a withdrawal, and a payment on the card is a deposit.

    This file is for {process.transaction_source.name}.

    {process.file.filename}
    {process.file.raw_content}
    """.strip())]




def already_processed(process: InProcessFile) -> bool:
    val = process.session.query(ProcessFileJob).filter(ProcessFileJob.id == process.job.id, ProcessFileJob.status == JobStatus.completed).one_or_none()
    return bool(val)


def apply_upload_config(process: InProcessFile) -> InProcessFile:
    config: None | UploadConfiguration = None 

    logger.info(f"Applying upload configuration for file: {process.file.filename}")

    if process.job.config_id:
        config = process.session.query(UploadConfiguration).filter(
            UploadConfiguration.id == process.job.config_id
        ).first()

    if not config:
        query = select(UploadConfiguration).where(
                text(":filename_raw_text ~* filename_regex"), 
                UploadConfiguration.user_id == process.user.id
            )
        config = process.session.execute(query, {"filename_raw_text": process.file.filename + process.file.raw_text}).scalars().first()
    
    if not config:
        config = create_configurations(process)

    assert config, "Should have generated a config by now"

    transaction_source = process.session.query(TransactionSource).filter(TransactionSource.id == config.transaction_source_id)

    return replace(process, config=config, transaction_source=transaction_source)

def remove_transactions_if_necessary(process: InProcessFile) -> InProcessFile:
    """Remove existing transactions if the file has been processed before."""
    if already_processed(process):
        logger.info(f"Removing previous transactions for file: {process.file.filename}")
        process.session.query(ProcessFileJob).filter()
    return process


def request_llm_parse_of_transactions(process: InProcessFile) -> InProcessFile:
    """Request AI to parse transactions from the extracted text."""
    if process.config is None:
        raise ValueError("Upload configuration is required before parsing transactions.")

    parsed_transactions = make_chat_request(TransactionsWrapper, generate_transactions_prompt(process))

    return replace(process, parsed_transactions=parsed_transactions)

