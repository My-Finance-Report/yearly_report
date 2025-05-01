from typing import NewType
import enum

from app.models.models import Base
from sqlalchemy import (
    Boolean,
    Enum,
    ForeignKey,
    UniqueConstraint,
    Text,
    Integer,
)
from sqlalchemy.orm import Mapped, mapped_column

from app.models.user import UserId


TransactionSourceId = NewType("TransactionSourceId", int)


class SourceKind(str, enum.Enum):
    account = "account"
    investment = "investment"
    card = "card"


class TransactionSource(Base):
    __tablename__ = "transaction_source"

    id: Mapped[TransactionSourceId] = mapped_column(
        Integer, primary_key=True, autoincrement=True
    )
    name: Mapped[str] = mapped_column(Text, nullable=False)
    user_id: Mapped[UserId] = mapped_column(ForeignKey("user.id"), nullable=False)
    archived: Mapped[bool] = mapped_column(Boolean, default=False)
    source_kind: Mapped[SourceKind] = mapped_column(
        Enum(SourceKind), default=SourceKind.account
    )
    plaid_account_id: Mapped[int | None] = mapped_column(
        ForeignKey("plaid_account.id"), nullable=True
    )

    __table_args__ = (
        UniqueConstraint("user_id", "name", name="uq_transaction_source"),
    )
