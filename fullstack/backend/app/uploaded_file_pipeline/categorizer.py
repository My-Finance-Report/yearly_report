import logging
from typing import  List, Type 

from app.open_ai_utils import ChatMessage, Prompt, make_chat_request
from app.uploaded_file_pipeline.local_types import InProcessFile
from func_utils import make_batches
from pydantic import BaseModel, Field, create_model
from typing import List, Literal




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



def generate_categorization_prompt(categories: list[str], transactions: list[str]) -> Prompt:

    return [ChatMessage(role="user", message=f"""
    Here is a list of categories:

    {", ".join(categories)}   

    Assign the most appropriate category to the following transactions: 
    
    {"\n".join(transactions)}

    """.strip())]


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
