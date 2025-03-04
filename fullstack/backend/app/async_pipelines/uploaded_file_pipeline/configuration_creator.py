import logging

from sqlalchemy.orm import Session

from app.async_pipelines.uploaded_file_pipeline.local_types import (
    InProcessFile,
    PartialAccountCategoryConfig,
    PartialUploadConfig,
)
from app.models import (
    Category,
    SourceKind,
    TransactionSource,
    UploadConfiguration,
    User,
)
from app.open_ai_utils import ChatMessage, make_chat_request

# Logging setup
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


def generate_upload_config_prompt(pdf_content: str) -> str:
    """Generates the AI prompt for extracting upload configuration."""
    return f"""
    Analyze the following PDF content to extract an upload configuration.

    - Identify stable keywords that will work across multiple months.
    - Extract start and end keywords that isolate this month's transactions.
    - Avoid including summary sections.

    ### PDF Content:
    {pdf_content}
    """.strip()


def generate_account_category_prompt(pdf_content: str) -> str:
    return f"""
    Analyze the following PDF content to determine account categorization.

    - Extract a stable account name.
    - Identify if it's an Investment, Card, or Account.
    - List relevant transaction categories.

    ### Expected JSON Format:
    {{
      "name": "<Descriptive name>",
      "kind": "investment" | "card" | "account",
      "categories": ["<Category1>", "<Category2>", "<Category3>"]
    }}

    ### PDF Content:
    {pdf_content}
    """.strip()


def extract_account_and_categories(
    pdf_content: str,
) -> PartialAccountCategoryConfig | None:
    prompt = generate_account_category_prompt(pdf_content)

    messages = [ChatMessage(role="user", content=prompt)]
    response = make_chat_request(PartialAccountCategoryConfig, messages)

    if response:
        return response
    else:
        logger.error("Failed to extract account categories.")
        return None


def generate_upload_configuration(
    session: Session,
    user: User,
    transaction_source: TransactionSource,
    pdf_content: str,
) -> UploadConfiguration | None:
    prompt = generate_upload_config_prompt(pdf_content)

    messages = [ChatMessage(role="user", content=prompt)]
    response = make_chat_request(PartialUploadConfig, messages)

    if not response:
        logger.error("Failed to extract upload configuration.")
        return None

    upload_config = UploadConfiguration(
        filename_regex=response.fileIdKeyword,
        start_keyword=response.startKeyword,
        end_keyword=response.endKeyword,
        transaction_source_id=transaction_source.id,
        user_id=user.id,
    )

    session.add(upload_config)
    session.commit()
    return upload_config


HARDCODED_CATEGORIES: dict[SourceKind, list[str]] = {
    SourceKind.account: [
        "Income",
        "Investments",
        "Credit Card Payments",
        "Transfers",
        "Housing",
    ],
    SourceKind.card: [
        "Groceries",
        "Travel",
        "Gas",
        "Insurance",
        "Misc",
        "Subscriptions",
        "Credit Card Payments",
        "Entertainment",
    ],
    SourceKind.investment: ["Stocks", "Bonds", "Index Funds"],
}
USE_HARDCODED = True


def save_account_config(
    session: Session, user: User, account_config: PartialAccountCategoryConfig
) -> TransactionSource:
    existing = (
        session.query(TransactionSource)
        .filter(
            TransactionSource.name == account_config.name,
            TransactionSource.user_id == user.id,
        )
        .one_or_none()
    )
    if existing:
        return existing

    transaction_source = TransactionSource(
        source_kind=account_config.kind, name=account_config.name, user_id=user.id
    )
    session.add(transaction_source)
    session.commit()

    cats_to_use = (
        HARDCODED_CATEGORIES[transaction_source.source_kind]
        if USE_HARDCODED
        else account_config.categories
    )

    categories = [
        Category(name=cat, source_id=transaction_source.id, user_id=user.id)
        for cat in cats_to_use
    ]
    session.add_all(categories)
    session.commit()

    return transaction_source


def create_configurations(process: InProcessFile) -> UploadConfiguration:
    session = process.session
    user = process.user
    pdf_content = process.file.raw_content

    account_config = extract_account_and_categories(pdf_content)
    if not account_config:
        raise ValueError("Failed to extract account information.")

    transaction_source = save_account_config(session, user, account_config)

    existing_config_with_bad_keywords = (
        session.query(UploadConfiguration)
        .filter(
            UploadConfiguration.user_id == user.id,
            UploadConfiguration.transaction_source_id == transaction_source.id,
        )
        .one_or_none()
    )

    upload_config: UploadConfiguration | None = None
    if existing_config_with_bad_keywords:
        print(
            f"**** existing config id {existing_config_with_bad_keywords.id} found with bad keywords**** "
        )
        upload_config = existing_config_with_bad_keywords
    else:
        upload_config = generate_upload_configuration(
            session, user, transaction_source, pdf_content
        )
        if not upload_config:
            raise ValueError("Failed to extract upload configuration.")

    return upload_config
