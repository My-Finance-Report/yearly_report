import logging
from dataclasses import replace
from datetime import datetime
from typing import cast

from app.async_pipelines.uploaded_file_pipeline.local_types import (
    CategorizedTransaction,
    InProcessFile,
    PartialTransaction,
    Recategorization,
    create_categorized_transactions_wrapper,
)
from app.func_utils import make_batches
from app.models import Transaction
from app.open_ai_utils import ChatMessage, Prompt, make_chat_request

logging.basicConfig(level=logging.INFO)

logger = logging.getLogger(__name__)


def generate_categorization_prompt(
    categories: list[str],
    transactions: list[PartialTransaction],
    previous_recategorizations: list[Recategorization] | None = None,
) -> Prompt:
    return [
        ChatMessage(
            role="user",
            content=f"""
    Here is a list of categories:

    {", ".join(categories)}

    Here is a list of previous overwrites the user has responded with:

    {"\n".join([f"{r.description}: {r.previous_category} changed to {r.overrided_category}" for r in previous_recategorizations]) if previous_recategorizations else "None"}

    Assign the most appropriate category to the following transactions:

    {"\n".join([t.model_dump_json() for t in transactions])}

    """.strip(),
        )
    ]


class TransactionsCoerceType:
    """
    helpful as we are doing some type gymnastics to have
    generic api calls with dynamic runtime variables
    """

    transactions: list[CategorizedTransaction]


def update_filejob_with_nickname(in_process: InProcessFile) -> InProcessFile:
    assert in_process.transaction_source, "must have"
    assert in_process.transactions, "must have"

    dates = [
        t.partialTransactionDateOfTransaction
        for t in in_process.transactions.transactions
    ]

    start_date = min(dates)
    end_date = max(dates)
    nickname = f"{in_process.transaction_source.name}-{start_date}-{end_date}"
    in_process.file.nickname = nickname
    in_process.session.add(in_process.file)
    return in_process


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
            categorized = cast(
                TransactionsCoerceType,
                make_chat_request(
                    CategorizedTransactionsWrapper,
                    generate_categorization_prompt(
                        categories=account_categories,
                        transactions=batch,
                        previous_recategorizations=process.previous_recategorizations,
                    ),
                ),
            )
            if categorized:
                out.extend(categorized.transactions)
        except Exception as e:
            logger.error(f"Failed to categorize batch: {e}")

    return replace(process, categorized_transactions=out)


def insert_categorized_transactions(in_process: InProcessFile) -> None:
    assert in_process.transaction_source, "must have"
    assert in_process.categorized_transactions, "must have"
    assert in_process.categories, "must have"

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
