import logging
from typing import Optional
from app.uploaded_file_pipeline.local_types import InProcessFile, PartialAccountCategoryConfig, PartialUploadConfig
from sqlalchemy.orm import Session
from app.open_ai_utils import ChatMessage, make_chat_request
from app.models import Category, UploadConfiguration, User, TransactionSource

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
      "kind": "Investment" | "Card" | "Account",
      "categories": ["<Category1>", "<Category2>", "<Category3>"]
    }}

    ### PDF Content:
    {pdf_content}
    """.strip()




def extract_account_and_categories(session: Session, user: User, pdf_content: str) -> Optional[PartialAccountCategoryConfig]:
    prompt = generate_account_category_prompt(pdf_content)

    messages = [ChatMessage(role="user", content=prompt)]
    response = make_chat_request(PartialAccountCategoryConfig, messages)

    if response:
        return response
    else:
        logger.error("Failed to extract account categories.")
        return None


def extract_upload_configuration(session: Session, user: User, transaction_source: TransactionSource, pdf_content: str) -> Optional[UploadConfiguration]:
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


def save_account_config(session: Session, user: User, account_config: PartialAccountCategoryConfig) -> TransactionSource:
    transaction_source = TransactionSource(kind=account_config.kind, name=account_config.name, user_id=user.id)
    session.add(transaction_source)
    session.commit()  

    categories = [Category(name=cat, source_id=transaction_source.id, user_id=user.id) for cat in account_config.categories]
    session.add_all(categories) 
    session.commit()  

    return transaction_source

    

def create_configurations(process: InProcessFile)-> UploadConfiguration:
    # TODO just pass the process object around
    session = process.session
    user = process.user
    pdf_content = process.file.raw_content

    try:
        account_config = extract_account_and_categories(session, user, pdf_content)
        if not account_config:
            raise ValueError("Failed to extract account information.")

        transaction_source = save_account_config(session, user, account_config)

        upload_config = extract_upload_configuration(session, user, transaction_source, pdf_content)
        if not upload_config:
            raise ValueError("Failed to extract upload configuration.")

        return upload_config

    except Exception as e:
        session.rollback()  # Rollback changes on failure
        logger.error(f"Error creating configurations: {e}")
        raise
