import logging
from typing import  List

from app.open_ai_utils import ChatMessage, Prompt, make_chat_request
from app.uploaded_file_pipeline.local_types import CategorizedTransaction, InProcessFile, create_categorized_transactions_wrapper
from func_utils import make_batches
from typing import List, Literal




# Logging setup
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


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
