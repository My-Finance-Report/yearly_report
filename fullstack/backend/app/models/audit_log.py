from datetime import datetime, timezone
from typing import NewType
import enum

from pydantic import BaseModel, Field
from app.models.category import CategoryId
from app.models.models import Base, JSONType
from sqlalchemy import (
    Boolean,
    Enum,
    DateTime,
    ForeignKey,
    Text,
    Integer,
)
from sqlalchemy.orm import Mapped, mapped_column
from sqlalchemy import DateTime
from datetime import datetime
from app.models.transaction import TransactionId, TransactionKind

from app.models.user import UserId


AuditLogId = NewType("AuditLogId", int)


class AuditLogAction(str, enum.Enum):
    reclassify_transaction_kind = "reclassify_transaction_kind"
    reclassify_transaction_category = "reclassify_transaction_category"
    change_date = "change_date"
    change_amount = "change_amount"


class AuditChange(BaseModel):
    old_date: datetime | None = Field(default=None)
    new_date: datetime | None = Field(default=None)
    old_amount: float | None = Field(default=None)
    new_amount: float | None = Field(default=None)
    old_category: CategoryId | None = Field(default=None)
    new_category: CategoryId | None = Field(default=None)
    old_kind: TransactionKind | None = Field(default=None)
    new_kind: TransactionKind | None = Field(default=None)


class AuditLog(Base):
    __tablename__ = "audit_log"

    id: Mapped[AuditLogId] = mapped_column(
        Integer, primary_key=True, autoincrement=True
    )
    user_id: Mapped[UserId] = mapped_column(ForeignKey("user.id"), nullable=False)
    action: Mapped[AuditLogAction] = mapped_column(Enum(AuditLogAction), nullable=False)
    change: Mapped[AuditChange] = mapped_column(JSONType(AuditChange), nullable=False)
    apply_to_future: Mapped[bool] = mapped_column(Boolean, nullable=False)
    transaction_id: Mapped[TransactionId | None] = mapped_column(
        ForeignKey("transaction.id"), nullable=True
    )
    details: Mapped[str | None] = mapped_column(Text, nullable=True)
    created_at: Mapped[datetime] = mapped_column(
        DateTime, default=lambda: datetime.now(timezone.utc)
    )
