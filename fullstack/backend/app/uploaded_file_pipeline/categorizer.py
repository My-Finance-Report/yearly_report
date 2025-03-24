import logging
from dataclasses import dataclass, replace
from datetime import datetime
from typing import cast

from app.async_pipelines.uploaded_file_pipeline.local_types import (
    CategorizedTransaction,
    InProcessFile,
    PartialTransaction,
    create_categorized_transactions_wrapper,
)
from app.func_utils import make_batches
from app.open_ai_utils import ChatMessage, Prompt, make_chat_request

from ..models import Transaction

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


def generate_categorization_prompt(
    categories: list[str], transactions: list[PartialTransaction]
) -> Prompt:
    return [
        ChatMessage(
            role="user",
            content=f"""
    Here is a list of categories:

    {", ".join(categories)}

    Assign the most appropriate category to the following transactions:

    {"\n".join([t.model_dump_json() for t in transactions])}

    """.strip(),
        )
    ]


@dataclass
class _WrapperClass:
    """helper for dealing with abstract types"""

    transactions: list[CategorizedTransaction]


def categorize_extracted_transactions(process: InProcessFile) -> InProcessFile:
    assert process.transactions, "didnt find transactions"
    assert process.categories, "must have"

    logger.info(
        f"Categorizing {len(process.transactions.transactions)} transactions..."
    )
    account_categories = [cat.name for cat in process.categories]

    CategorizedTransactionsWrapper = create_categorized_transactions_wrapper(
        account_categories
    )

    out: list[CategorizedTransaction] = []
    batch: list[PartialTransaction]
    for batch in make_batches(process.transactions.transactions):
        try:
            categorized: _WrapperClass = cast(
                _WrapperClass,
                make_chat_request(
                    CategorizedTransactionsWrapper,
                    generate_categorization_prompt(
                        categories=account_categories, transactions=batch
                    ),
                ),
            )
            if categorized:
                out.extend(categorized.transactions)
        except Exception as e:
            logger.error(f"Failed to categorize batch: {e}")

    return replace(process, categorized_transactions=out)


def insert_categorized_transactions(in_process: InProcessFile) -> None:
    assert in_process.categories, "must have"
    assert in_process.transaction_source, "must have"
    assert in_process.file, "must have"
    assert in_process.categorized_transactions, "must have"

    category_lookup = {cat.name: cat.id for cat in in_process.categories}

    transactions_to_insert = [
        Transaction(
            description=t.partialTransactionDescription,
            category_id=category_lookup[t.category],
            date_of_transaction=datetime.strptime(
                t.partialTransactionDateOfTransaction, "%m/%d/%Y"
            ),
            amount=t.partialTransactionAmount,
            transaction_source_id=in_process.transaction_source.id,
            kind=t.partialTransactionKind,
            uploaded_pdf_id=in_process.file.id,
            user_id=in_process.user.id,
            archived=False,
        )
        for t in in_process.categorized_transactions
    ]

    in_process.session.bulk_save_objects(transactions_to_insert)
    in_process.session.commit()
