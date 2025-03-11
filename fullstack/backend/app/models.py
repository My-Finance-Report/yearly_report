import enum
from datetime import datetime, timezone
from decimal import Decimal
from typing import NewType

from pydantic import BaseModel, Field
from sqlalchemy import (
    JSON,
    Boolean,
    DateTime,
    Enum,
    ForeignKey,
    Integer,
    Numeric,
    String,
    Text,
    UniqueConstraint,
)
from sqlalchemy.orm import DeclarativeBase, Mapped, mapped_column, relationship


class Base(DeclarativeBase):
    __rls_enabled__ = True


class SourceKind(str, enum.Enum):
    account = "account"
    investment = "investment"
    card = "card"


class TransactionKind(str, enum.Enum):
    withdrawal = "withdrawal"
    deposit = "deposit"


class JobStatus(str, enum.Enum):
    completed = "completed"
    pending = "pending"
    processing = "processing"
    failed = "failed"


class JobKind(str, enum.Enum):
    full_upload = "full_upload"
    recategorize = "recategorize"


UserId = NewType("UserId", int)
TransactionId = NewType("TransactionId", int)
CategoryId = NewType("CategoryId", int)
TransactionSourceId = NewType("TransactionSourceId", int)
UploadConfigurationId = NewType("UploadConfigurationId", int)
ProcessFileJobId = NewType("ProcessFileJobId", int)
UploadedPdfId = NewType("UploadedPdfId", int)
ColChartConfigId = NewType("ColChartConfigId", int)
BudgetId = NewType("BudgetId", int)
BudgetCategoryLinkId = NewType("BudgetCategoryLinkId", int)
BudgetEntryId = NewType("BudgetEntryId", int)
AuditLogId = NewType("AuditLogId", int)


class User(Base):
    __tablename__ = "user"

    id: Mapped[UserId] = mapped_column(Integer, primary_key=True, autoincrement=True)
    email: Mapped[str] = mapped_column(String, unique=True, nullable=False)
    hashed_password: Mapped[str] = mapped_column(
        String, nullable=False
    )  # Renamed for clarity
    full_name: Mapped[str | None] = mapped_column(
        String, nullable=True
    )  # From the other model
    is_active: Mapped[bool] = mapped_column(
        Boolean, default=True
    )  # Default user is active
    is_superuser: Mapped[bool] = mapped_column(
        Boolean, default=False
    )  # Flag for admin users
    created_at: Mapped[datetime] = mapped_column(
        DateTime, default=lambda: datetime.now(timezone.utc)
    )
    onboarding_step: Mapped[int | None] = mapped_column(Integer, nullable=True)

    sessions: Mapped[list["UserSession"]] = relationship(back_populates="user")


class UserSession(Base):
    __tablename__ = "user_session"

    id: Mapped[int] = mapped_column(Integer, primary_key=True, autoincrement=True)
    user_id: Mapped[UserId] = mapped_column(ForeignKey("user.id"), nullable=False)
    session_token: Mapped[str] = mapped_column(Text, unique=True, nullable=False)
    expires_at: Mapped[datetime] = mapped_column(DateTime, nullable=False)

    user: Mapped["User"] = relationship(back_populates="sessions")


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

    __table_args__ = (
        UniqueConstraint("user_id", "name", name="uq_transaction_source"),
    )


class Category(Base):
    __tablename__ = "category"

    id: Mapped[CategoryId] = mapped_column(
        Integer, primary_key=True, autoincrement=True
    )
    name: Mapped[str] = mapped_column(Text, nullable=False)
    source_id: Mapped[TransactionSourceId] = mapped_column(
        ForeignKey("transaction_source.id"), nullable=False
    )
    user_id: Mapped[UserId] = mapped_column(ForeignKey("user.id"), nullable=False)
    archived: Mapped[bool] = mapped_column(Boolean, default=False)

    __table_args__ = (UniqueConstraint("name", "source_id", name="uq_category"),)


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


class UploadedPdf(Base):
    __tablename__ = "uploaded_pdf"

    id: Mapped[UploadedPdfId] = mapped_column(
        Integer, primary_key=True, autoincrement=True
    )
    filename: Mapped[str] = mapped_column(Text, nullable=False)
    nickname: Mapped[str | None] = mapped_column(Text, nullable=True)
    raw_content: Mapped[str] = mapped_column(Text, nullable=False)
    raw_content_hash: Mapped[str] = mapped_column(Text, nullable=False)
    upload_time: Mapped[datetime] = mapped_column(DateTime, nullable=False)
    user_id: Mapped[UserId] = mapped_column(ForeignKey("user.id"), nullable=False)
    archived: Mapped[bool] = mapped_column(Boolean, default=False)

    __table_args__ = (
        UniqueConstraint("raw_content_hash", "user_id", name="uq_uploaded_pdf"),
    )


class UploadConfiguration(Base):
    __tablename__ = "upload_configuration"

    id: Mapped[UploadConfigurationId] = mapped_column(
        Integer, primary_key=True, autoincrement=True
    )
    filename_regex: Mapped[str] = mapped_column(Text, nullable=False)
    start_keyword: Mapped[str | None] = mapped_column(Text, nullable=True)
    end_keyword: Mapped[str | None] = mapped_column(Text, nullable=True)
    transaction_source_id: Mapped[TransactionSourceId] = mapped_column(
        ForeignKey("transaction_source.id"), nullable=False
    )
    user_id: Mapped[UserId] = mapped_column(ForeignKey("user.id"), nullable=False)

    __table_args__ = (
        UniqueConstraint("transaction_source_id", name="uq_upload_configuration"),
    )


class SankeyConfig(Base):
    __tablename__ = "sankey_config"

    id: Mapped[int] = mapped_column(Integer, primary_key=True, autoincrement=True)
    name: Mapped[str] = mapped_column(Text, nullable=False)
    user_id: Mapped[UserId] = mapped_column(ForeignKey("user.id"), nullable=False)


class SankeyInput(Base):
    __tablename__ = "sankey_input"

    id: Mapped[int] = mapped_column(Integer, primary_key=True, autoincrement=True)
    config_id: Mapped[int] = mapped_column(
        ForeignKey("sankey_config.id"), nullable=False
    )
    category_id: Mapped[CategoryId] = mapped_column(
        ForeignKey("category.id"), nullable=False
    )


class SankeyLinkage(Base):
    __tablename__ = "sankey_linkage"

    id: Mapped[int] = mapped_column(Integer, primary_key=True, autoincrement=True)
    config_id: Mapped[int] = mapped_column(
        ForeignKey("sankey_config.id"), nullable=False
    )
    category_id: Mapped[CategoryId] = mapped_column(
        ForeignKey("category.id"), nullable=False
    )
    target_source_id: Mapped[TransactionSourceId] = mapped_column(
        ForeignKey("transaction_source.id"), nullable=False
    )


class ColChartConfig(Base):
    __tablename__ = "col_chart_config"

    id: Mapped[ColChartConfigId] = mapped_column(
        Integer, primary_key=True, autoincrement=True
    )
    name: Mapped[str] = mapped_column(Text, nullable=False)
    user_id: Mapped[UserId] = mapped_column(ForeignKey("user.id"), nullable=False)
    active: Mapped[bool] = mapped_column(Boolean, nullable=False)

    __table_args__ = (
        UniqueConstraint("user_id", "active", name="uq_col_chart_config"),
    )


class ProcessFileJob(Base):
    __tablename__ = "process_file_job"

    id: Mapped[ProcessFileJobId] = mapped_column(
        Integer, primary_key=True, autoincrement=True
    )
    created_at: Mapped[datetime] = mapped_column(
        DateTime, default=lambda: datetime.now(timezone.utc)
    )
    last_tried_at: Mapped[datetime | None] = mapped_column(DateTime, nullable=True)
    status: Mapped[JobStatus] = mapped_column(Enum(JobStatus), nullable=False)
    user_id: Mapped[UserId] = mapped_column(ForeignKey("user.id"), nullable=False)
    config_id: Mapped[UploadConfigurationId | None] = mapped_column(
        ForeignKey("upload_configuration.id"), nullable=True
    )
    pdf_id: Mapped[UploadedPdfId] = mapped_column(
        ForeignKey("uploaded_pdf.id"), nullable=False
    )
    archived: Mapped[bool] = mapped_column(Boolean, default=False)
    attempt_count: Mapped[int] = mapped_column(Integer, default=0)
    error_messages: Mapped[str] = mapped_column(Text, nullable=True)
    kind: Mapped[JobKind] = mapped_column(Enum(JobKind), nullable=False)

    __table_args__ = (UniqueConstraint("pdf_id", name="uq_process_file_job"),)


class Budget(Base):
    __tablename__ = "budget"

    id: Mapped[BudgetId] = mapped_column(Integer, primary_key=True, autoincrement=True)
    name: Mapped[str] = mapped_column(Text, nullable=False)
    user_id: Mapped[UserId] = mapped_column(ForeignKey("user.id"), nullable=False)
    active: Mapped[bool] = mapped_column(Boolean, nullable=False)

    __table_args__ = (UniqueConstraint("user_id", "active", name="uq_budget"),)


class BudgetEntry(Base):
    __tablename__ = "budget_entry"

    id: Mapped[BudgetEntryId] = mapped_column(
        Integer, primary_key=True, autoincrement=True
    )
    user_id: Mapped[UserId] = mapped_column(ForeignKey("user.id"), nullable=False)
    name: Mapped[str] = mapped_column(Text, nullable=False)
    amount: Mapped[Decimal] = mapped_column(Numeric, nullable=False)
    budget_id: Mapped[BudgetId] = mapped_column(ForeignKey("budget.id"), nullable=False)


class BudgetCategoryLink(Base):
    __tablename__ = "budget_category_link"

    id: Mapped[BudgetCategoryLinkId] = mapped_column(
        Integer, primary_key=True, autoincrement=True
    )
    user_id: Mapped[UserId] = mapped_column(ForeignKey("user.id"), nullable=False)
    budget_entry_id: Mapped[BudgetEntryId] = mapped_column(
        ForeignKey("budget_entry.id"), nullable=False
    )
    category_id: Mapped[CategoryId] = mapped_column(
        ForeignKey("category.id"), nullable=False
    )


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
    change: Mapped[AuditChange] = mapped_column(JSON, nullable=False)
    apply_to_future: Mapped[bool | None] = mapped_column(Boolean, nullable=True)
    transaction_id: Mapped[TransactionId] = mapped_column(
        ForeignKey("transaction.id"), nullable=False
    )
    details: Mapped[str | None] = mapped_column(Text, nullable=True)
    created_at: Mapped[datetime] = mapped_column(
        DateTime, default=lambda: datetime.now(timezone.utc)
    )
