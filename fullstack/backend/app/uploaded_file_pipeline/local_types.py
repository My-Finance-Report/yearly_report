from dataclasses import dataclass
from typing import List, Literal, Type
from app.db import Session
from pydantic import BaseModel, Field, create_model
from app.models import  ProcessFileJob, TransactionSource, UploadedPdf, User, UploadConfiguration


class PdfParseException(Exception):
    pass


class PartialTransaction(BaseModel):
    partialTransactionDateOfTransaction: str = Field(..., description="Transaction date in MM/DD/YYYY format")
    partialTransactionDescription: str = Field(..., description="Description of the transaction")
    partialTransactionKind: Literal["Withdrawal", "Deposit"] = Field(..., description="Type of transaction")
    partialTransactionAmount: float = Field(..., description="Amount of the transaction")

class PartialUploadConfig(BaseModel):
    fileIdKeyword: str = Field(..., description="Stable keyword identifying this file type.")
    startKeyword: str = Field(..., description="Start keyword for transactions in the file.")
    endKeyword: str = Field(..., description="End keyword for transactions in the file.")


class PartialAccountCategoryConfig(BaseModel):
    name: str = Field(..., description="Stable name for the account type.")
    kind: Literal["Investment", "Card", "Account"] = Field(..., description="Type of account.")
    categories: List[str] = Field(..., description="List of relevant financial categories.")



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
    session:Session 
    user: User
    file: UploadedPdf
    job: ProcessFileJob
    config: UploadConfiguration | None = None
    transaction_source: TransactionSource | None = None
    transactions: TransactionsWrapper| None = None

