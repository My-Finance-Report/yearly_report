from dataclasses import dataclass
from typing import Literal

from pydantic import BaseModel, Field, create_model

from app.db import Session
from app.models import (
    Category,
    ProcessFileJob,
    Transaction,
    TransactionId,
    TransactionSource,
    UploadConfiguration,
    UploadedPdf,
    User,
)


class PdfParseException(Exception):
    pass


class PartialTransaction(BaseModel):
    partialTransactionId: TransactionId | None = Field(
        ..., description="Unique identifier for the transaction"
    )
    partialTransactionDateOfTransaction: str = Field(
        ..., description="Transaction date in MM/DD/YYYY format"
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


def create_categorized_transaction_model(categories: list[str]) -> type[BaseModel]:
    return create_model(
        "CategorizedTransaction",
        partialTransactionId=(
            TransactionId | None,
            Field(..., description="Unique identifier for the transaction"),
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


def create_categorized_transactions_wrapper(categories: list[str]) -> type[BaseModel]:
    # this type has a runtime generated enum for the allowed categories
    StrictCategorizedTransaction = create_categorized_transaction_model(categories)

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
class InProcessFile:
    session: Session
    user: User
    file: UploadedPdf | None = None
    job: ProcessFileJob | None = None
    config: UploadConfiguration | None = None
    transaction_source: TransactionSource | None = None
    categories: list[Category] | None = None
    transactions: TransactionsWrapper | None = None
    existing_transactions: list[Transaction] | None = None
    categorized_transactions: list[CategorizedTransaction] | None = None
    previous_recategorizations: list[Recategorization] | None = None
