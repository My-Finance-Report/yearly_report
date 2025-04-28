from typing import  NewType
import enum

from datetime import datetime, timezone
from app.models.category import CategoryId
from app.models.models import Base
from sqlalchemy import (
    Boolean,
    Enum,
    ForeignKey,
    Text,
    String,
    DateTime,
    Integer,
)
from sqlalchemy.orm import Mapped, mapped_column
from app.models.transaction_source import TransactionSourceId

from app.models.user import UserId


TransactionId = NewType("TransactionId", int)
PlaidTransactionId = NewType("PlaidTransactionId", int)

class TransactionKind(str, enum.Enum):
    withdrawal = "withdrawal"
    deposit = "deposit"


class Transaction(Base):
    __tablename__ = "transaction"

    id: Mapped[TransactionId] = mapped_column(
        Integer, primary_key=True, autoincrement=True
    )
    description: Mapped[str] = mapped_column(Text, nullable=False)
    category_id: Mapped[CategoryId] = mapped_column(
        ForeignKey("category.id"), nullable=False
    )
    date_of_transaction: Mapped[datetime] = mapped_column(DateTime, nullable=False)
    amount: Mapped[float] = mapped_column(Integer, nullable=False)
    transaction_source_id: Mapped[TransactionSourceId] = mapped_column(
        ForeignKey("transaction_source.id"), nullable=False
    )
    kind: Mapped[TransactionKind] = mapped_column(Enum(TransactionKind), nullable=False)
    uploaded_pdf_id: Mapped[int | None] = mapped_column(
        ForeignKey("uploaded_pdf.id"), nullable=True
    )
    user_id: Mapped[UserId] = mapped_column(ForeignKey("user.id"), nullable=False)
    archived: Mapped[bool] = mapped_column(Boolean, default=False)
    external_id: Mapped[PlaidTransactionId | None] = mapped_column(
        String, nullable=True, index=True
    )
    last_updated: Mapped[datetime | None] = mapped_column(
        DateTime, nullable=True, default=lambda: datetime.now(timezone.utc)
    )


