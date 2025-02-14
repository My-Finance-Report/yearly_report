from dataclasses import dataclass
import logging
from typing import Callable, List, Type, TypeVar
import subprocess

from fullstack.backend.app.open_ai_utils import ChatMessage, Prompt, make_chat_request
from sqlalchemy.orm import Session
from pydantic import BaseModel, Field, create_model
from typing import List, Literal



from app.models import ProcessFileJob, UploadedPdf, User, UploadConfiguration

# Logging setup
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

class PdfParseException(Exception):
    pass


class PartialTransaction(BaseModel):
    partialTransactionDateOfTransaction: str = Field(..., description="Transaction date in MM/DD/YYYY format")
    partialTransactionDescription: str = Field(..., description="Description of the transaction")
    partialTransactionKind: Literal["Withdrawal", "Deposit"] = Field(..., description="Type of transaction")
    partialTransactionAmount: float = Field(..., description="Amount of the transaction")


class CategorizedTransaction(PartialTransaction):
    category:  str = Field(...,description="The category of the transaction")


class TransactionsWrapper(BaseModel):
    transactions: List[PartialTransaction]

def create_categorized_transaction_model(categories: List[str]) -> Type[BaseModel]:

    return create_model(
        "CategorizedTransaction",
        partialTransactionDateOfTransaction=(str, Field(..., description="Transaction date in MM/DD/YYYY format")),
        partialTransactionDescription=(str, Field(..., description="Description of the transaction")),
        partialTransactionKind=(Literal["Withdrawal", "Deposit"], Field(..., description="Type of transaction")),
        partialTransactionAmount=(float, Field(..., description="Amount of the transaction")),
        category=(Literal[tuple(categories)], Field(..., description="The category of the transaction")),
    )


def create_categorized_transactions_wrapper(categories: List[str]) -> Type[BaseModel]:
    CategorizedTransaction = create_categorized_transaction_model(categories)

    return create_model(
        "CategorizedTransactionsWrapper",
        transactions=(List[CategorizedTransaction], Field(..., description="List of categorized transactions")),
    )


@dataclass(frozen=True)
class InProcessFile:
    session: Session
    user: User
    file: UploadedPdf
    job: ProcessFileJob
    config: UploadConfiguration | None = None
    transactions: TransactionsWrapper | None = None


def make_batches(data: List[str], batch_size: int = 10) -> List[List[str]]:
    return [data[i:i + batch_size] for i in range(0, len(data), batch_size)]


T = TypeVar("T")  
J = TypeVar("J")  

def safe_pipe(value: T, *funcs: Callable[[T], T], final: Callable[[T], J]) -> J:
    for func in funcs:
        try:
            value = func(value)
        except Exception as e:
            logger.error(f"Error in {func.__name__}: {e}")
            break  
    return value

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

def generate_categorization_prompt(categories: list[str], transactions: list[str]) -> Prompt:

    return [ChatMessage(role="user", message=f"""
    Here is a list of categories:

    {", ".join(categories)}   

    Assign the most appropriate category to the following transactions: 
    
    {"\n".join(transactions)}

    """.strip())]


def already_processed(process: InProcessFile) -> bool:
    """Check if the file has already been processed."""
    # Placeholder: Implement logic to check if transactions already exist.
    return False  # Assume not processed for now


def apply_upload_config(process: InProcessFile) -> InProcessFile:
    """Retrieve and apply upload configuration if it exists."""
    config = None  # Replace with actual DB lookup logic

    if not process.config:
        logger.info(f"Applying upload configuration for file: {process.file.filename}")
        # Fetch config from DB (replace this with actual DB call)
        config = process.session.query(UploadConfiguration).filter(
            UploadConfiguration.id == process.file.id
        ).first()

    return InProcessFile(
        session=process.session,
        user=process.user,
        file=process.file,
        config=config,
    )


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

    return InProcessFile(
        session=process.session,
        user=process.user,
        file=process.file,
        config=process.config,
        transactions=parsed_transactions,
    )


def categorize_extracted_transactions(process: InProcessFile) -> List[CategorizedTransaction]:
    if not process.transactions:
        logger.warning("No transactions found, skipping categorization.")
        return []

    logger.info(f"Categorizing {len(process.transactions)} transactions...")
    
    account_categories = ["Groceries", "Rent", "Entertainment", "Utilities"]
    CategorizedTransactionsWrapper = create_categorized_transactions_wrapper(account_categories)

    out: list[CategorizedTransaction] = []
    for batch in make_batches(process.transactions):
        try:
            categorized = make_chat_request(
                CategorizedTransactionsWrapper, 
                generate_categorization_prompt(categories=account_categories, transactions=batch)
            )
            if categorized:
                out.extend(categorized)
        except Exception as e:
            logger.error(f"Failed to categorize batch: {e}")

    return out



def pipe(value, *funcs):
    for func in funcs:
        value = func(value)
    return value


# Usage example:
def process_pdf_file(session: Session, user: User, file: UploadedPdf, job: ProcessFileJob) -> List[CategorizedTransaction]:
    """Process a PDF file and return categorized transactions (functional pipeline)."""
    in_process = InProcessFile(session=session, user=user, file=file, job=job)
    
    return safe_pipe(
        in_process,
        apply_upload_config,
        remove_transactions_if_necessary,
        request_llm_parse_of_transactions,
        final=categorize_extracted_transactions
    ) 
