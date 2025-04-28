from datetime import datetime, timezone
from typing import NewType

from pydantic import BaseModel
from app.models.models import Base, JSONType
from sqlalchemy import (
    Boolean,
    DateTime,
    ForeignKey,
    Text,
    Integer,
)
from sqlalchemy.orm import Mapped, mapped_column
from sqlalchemy import DateTime
from datetime import datetime
from app.models.transaction import TransactionKind
from app.models.transaction_source import SourceKind

from app.models.user import UserId


class TransactionBase(BaseModel):
    description: str
    category_id: int
    date_of_transaction: datetime
    amount: float
    transaction_source_id: int
    kind: TransactionKind
    uploaded_pdf_id: None | int = None
    archived: bool = False

    class Config:
        from_attributes = True
        orm_mode = True


class CategoryBase(BaseModel):
    name: str
    source_id: int
    archived: bool = False

    class Config:
        orm_mode = True


class TransactionSourceBase(BaseModel):
    name: str
    archived: bool = False
    source_kind: SourceKind = SourceKind.account

    class Config:
        orm_mode = True


class ReportData(BaseModel):
    transactions: list[TransactionBase]
    transaction_sources: list[TransactionSourceBase]
    categories: list[CategoryBase]


TransactionReportId = NewType("TransactionReportId", int)


class Report(Base):
    __tablename__ = "report"

    id: Mapped[TransactionReportId] = mapped_column(
        Integer, primary_key=True, autoincrement=True
    )
    user_id: Mapped[UserId] = mapped_column(ForeignKey("user.id"), nullable=False)
    name: Mapped[str] = mapped_column(Text, nullable=False)
    created_at: Mapped[datetime] = mapped_column(
        DateTime, default=lambda: datetime.now(timezone.utc)
    )
    expires_at: Mapped[datetime | None] = mapped_column(DateTime, nullable=True)
    public_token: Mapped[str | None] = mapped_column(Text, unique=True, nullable=True)
    report_data: Mapped[ReportData] = mapped_column(
        JSONType(ReportData), nullable=False
    )
    archived: Mapped[bool] = mapped_column(Boolean, default=False)
