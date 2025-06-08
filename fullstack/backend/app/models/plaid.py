from datetime import datetime, timezone
import enum
from typing import NewType
from app.models.models import Base
from sqlalchemy import (
    String,
    Boolean,
    Enum,
    DateTime,
    ForeignKey,
    Float,
    Text,
    Integer,
)
from sqlalchemy.orm import Mapped, mapped_column
from sqlalchemy import DateTime
from datetime import datetime
from app.models.transaction_source import TransactionSourceId

from app.models.user import UserId

PlaidItemId = NewType("PlaidItemId", int)
PlaidAccountId = NewType("PlaidAccountId", int)
PlaidAccountBalanceId = NewType("PlaidAccountBalanceId", int)
PlaidSyncLogId = NewType("PlaidSyncLogId", int)


class PlaidItem(Base):
    __tablename__ = "plaid_item"

    id: Mapped[PlaidItemId] = mapped_column(
        Integer, primary_key=True, autoincrement=True
    )
    user_id: Mapped[UserId] = mapped_column(ForeignKey("user.id"), nullable=False)
    plaid_item_id: Mapped[str] = mapped_column(String, nullable=False, unique=True)
    access_token: Mapped[str] = mapped_column(String, nullable=False)
    created_at: Mapped[datetime] = mapped_column(
        DateTime, default=lambda: datetime.now(timezone.utc)
    )


class PlaidAccountBalance(Base):
    __tablename__ = "plaid_account_balance"

    id: Mapped[PlaidAccountBalanceId] = mapped_column(
        Integer, primary_key=True, autoincrement=True
    )
    plaid_account_id: Mapped[PlaidAccountId] = mapped_column(
        ForeignKey("plaid_account.id"), nullable=True, index=True
    )
    transaction_source_id: Mapped[TransactionSourceId] = mapped_column(
        ForeignKey("transaction_source.id"), nullable=True, index=True
    )
    balance: Mapped[float] = mapped_column(Float, nullable=False)  # aka current
    available: Mapped[float | None] = mapped_column(Float, nullable=True)
    iso_currency_code: Mapped[str | None] = mapped_column(String, nullable=True)
    timestamp: Mapped[datetime] = mapped_column(
        DateTime, default=lambda: datetime.now(timezone.utc), nullable=False
    )


class PlaidAccount(Base):
    __tablename__ = "plaid_account"

    id: Mapped[PlaidAccountId] = mapped_column(
        Integer, primary_key=True, autoincrement=True
    )
    user_id: Mapped[UserId] = mapped_column(ForeignKey("user.id"), nullable=False)
    plaid_item_id: Mapped[PlaidItemId] = mapped_column(
        ForeignKey("plaid_item.id"), nullable=False
    )
    plaid_account_id: Mapped[str] = mapped_column(String, nullable=False, unique=True)
    name: Mapped[str] = mapped_column(String, nullable=False)
    mask: Mapped[str | None] = mapped_column(String, nullable=True)
    type: Mapped[str] = mapped_column(String, nullable=False)
    cursor: Mapped[str | None] = mapped_column(String, nullable=True, default=None)
    subtype: Mapped[str | None] = mapped_column(String, nullable=True)
<<<<<<< HEAD
    active: Mapped[bool | None] = mapped_column(
        Boolean, default=True, nullable=True
    )  # deprecated, use archived
||||||| 50226e46
    active: Mapped[bool | None] = mapped_column(Boolean, default=True, nullable=True)
=======
>>>>>>> a3d0e22de1244d2a04dc1d9a8bc043b27f2ed104
    archived: Mapped[bool] = mapped_column(Boolean, default=False, nullable=False)
    created_at: Mapped[datetime] = mapped_column(
        DateTime, default=lambda: datetime.now(timezone.utc)
    )


class SyncStatus(str, enum.Enum):
    SUCCESS = "success"
    FAILURE = "failure"


class PlaidSyncLog(Base):
    __tablename__ = "plaid_sync_log"

    id: Mapped[int] = mapped_column(Integer, primary_key=True, autoincrement=True)
    user_id: Mapped[UserId] = mapped_column(ForeignKey("user.id"), nullable=False)
    plaid_item_id: Mapped[PlaidItemId] = mapped_column(
        ForeignKey("plaid_item.id"), nullable=False
    )
    plaid_account_id: Mapped[PlaidAccountId | None] = mapped_column(
        ForeignKey("plaid_account.id"), nullable=True
    )
    sync_type: Mapped[str] = mapped_column(String, nullable=False)
    start_date: Mapped[datetime | None] = mapped_column(DateTime, nullable=True)
    end_date: Mapped[datetime | None] = mapped_column(DateTime, nullable=True)
    added_count: Mapped[int | None] = mapped_column(Integer, nullable=True)
    modified_count: Mapped[int | None] = mapped_column(Integer, nullable=True)
    removed_count: Mapped[int | None] = mapped_column(Integer, nullable=True)
    error_message: Mapped[str | None] = mapped_column(Text, nullable=True)
    status: Mapped[SyncStatus] = mapped_column(Enum(SyncStatus), nullable=True)
    created_at: Mapped[datetime] = mapped_column(
        DateTime, default=lambda: datetime.now(timezone.utc)
    )
