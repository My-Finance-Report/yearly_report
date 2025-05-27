from dataclasses import dataclass
from typing import Annotated, Any, Callable, Generic, Literal, Optional, TypeVar
from pydantic import BaseModel, Field, create_model

from app.db import Session
from app.models.category import Category
from app.models.transaction import PlaidTransactionId, Transaction, TransactionId
from app.models.transaction_source import TransactionSource
from app.models.upload_configuration import UploadConfiguration
from app.models.uploaded_pdf import UploadedPdf
from app.models.user import User
from app.models.worker_job import WorkerJob


class PdfParseException(Exception):
    pass


class PartialTransaction(BaseModel):
    partialTransactionId: TransactionId | None = Field(
        ..., description="Unique identifier for the transaction"
    )
    partialPlaidTransactionId: PlaidTransactionId | None = Field(
        ..., description="Plaid transaction identifier"
    )
    partialTransactionDateOfTransaction: str = Field(
        ..., description="Transaction date in %m/%d/%Y format"
    )
    partialTransactionDescription: str = Field(
        ..., description="Description of the transaction"
    )
    partialTransactionKind: Literal["withdrawal", "deposit"] = Field(
        ..., description="Type of transaction"
    )
    partialTransactionAmount: float = Field(
        ..., description="Amount of the transaction"
    )


class PartialUploadConfig(BaseModel):
    filenameRegex: str = Field(
        ..., description="Stable keyword identifying this file type."
    )
    startKeyword: str = Field(
        ..., description="Start keyword for transactions in the file."
    )
    endKeyword: str = Field(
        ..., description="End keyword for transactions in the file."
    )


class PartialAccountCategoryConfig(BaseModel):
    name: str = Field(..., description="Stable name for the account type.")
    kind: Literal["investment", "card", "account"] = Field(
        ..., description="Type of account."
    )
    categories: list[str] = Field(
        ..., description="List of relevant financial categories."
    )


class CategorizedTransaction(PartialTransaction):
    category: str = Field(..., description="The category of the transaction")


class TransactionsWrapper(BaseModel):
    transactions: list[PartialTransaction]


def create_categorized_transaction_model(
    categories: list[str],
    plaidTransactionIds: list[PlaidTransactionId],
    transactionIds: list[TransactionId],
) -> type[BaseModel]:
    return create_model(
        "CategorizedTransaction",
        partialTransactionId=(
            Literal[tuple(transactionIds)] if transactionIds else None,
            Field(..., description="Unique identifier for the transaction"),
        ),
        partialPlaidTransactionId=(
            Literal[tuple(plaidTransactionIds)] if plaidTransactionIds else None,
            Field(..., description="Plaid transaction identifier"),
        ),
        partialTransactionDateOfTransaction=(
            str,
            Field(..., description="Transaction date in MM/DD/YYYY format"),
        ),
        partialTransactionDescription=(
            str,
            Field(..., description="Description of the transaction"),
        ),
        partialTransactionKind=(
            Literal["withdrawal", "deposit"],
            Field(..., description="Type of transaction"),
        ),
        partialTransactionAmount=(
            float,
            Field(..., description="Amount of the transaction"),
        ),
        category=(
            Literal[tuple(categories)],
            Field(..., description="The category of the transaction"),
        ),
    )


def create_categorized_transactions_wrapper(
    categories: list[str],
    plaidTransactionIds: list[PlaidTransactionId],
    transactionIds: list[TransactionId],
) -> type[BaseModel]:
    # this type has a runtime generated enum for the allowed categories
    StrictCategorizedTransaction = create_categorized_transaction_model(
        categories, plaidTransactionIds, transactionIds
    )

    return create_model(
        "CategorizedTransactionsWrapper",
        transactions=(
            list[StrictCategorizedTransaction],  # type: ignore[valid-type]
            Field(..., description="List of categorized transactions"),
        ),
    )


@dataclass
class Recategorization:
    description: str
    previous_category: str
    overrided_category: str


@dataclass(frozen=True)
class InProcessJob:
    session: Session
    user: User
    batch_id: str
    file: UploadedPdf | None = None
    job: WorkerJob | None = None
    config: UploadConfiguration | None = None
    transaction_source: TransactionSource | None = None
    categories: list[Category] | None = None
    transactions: TransactionsWrapper | None = None
    transactions_to_delete: list[PlaidTransactionId] | None = None
    existing_transactions: list[Transaction] | None = None
    categorized_transactions: list[CategorizedTransaction] | None = None
    inserted_transactions: list[Transaction] | None = None
    previous_recategorizations: list[Recategorization] | None = None
