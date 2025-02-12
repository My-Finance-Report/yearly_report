from sqlalchemy import (
    Boolean,
    Text,
    DateTime,
    Integer,
    String,
    Enum,
    ForeignKey,
    UniqueConstraint,
)
from typing import Optional, List
from datetime import datetime, timezone
import enum
from sqlalchemy.orm import DeclarativeBase, relationship, Mapped, mapped_column


class Base(DeclarativeBase):
    pass


class SourceKind(str, enum.Enum):
    account = "account"
    investment = "investment"
    card = "card"


class TransactionKind(str, enum.Enum):
    withdrawal = "Withdrawal"
    deposit = "Deposit"


class JobStatus(str, enum.Enum):
    completed = "completed"
    pending = "pending"
    processing = "processing"
    failed = "failed"


class User(Base):
    __tablename__ = "user"

    id: Mapped[int] = mapped_column(Integer, primary_key=True, autoincrement=True)
    email: Mapped[str] = mapped_column(String, unique=True, nullable=False)
    hashed_password: Mapped[str] = mapped_column(
        String, nullable=False
    )  # Renamed for clarity
    full_name: Mapped[Optional[str]] = mapped_column(
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
    onboarding_step: Mapped[Optional[int]] = mapped_column(Integer, nullable=True)

    sessions: Mapped[List["UserSession"]] = relationship(back_populates="user")


class UserSession(Base):
    __tablename__ = "user_session"

    id: Mapped[int] = mapped_column(Integer, primary_key=True, autoincrement=True)
    user_id: Mapped[int] = mapped_column(ForeignKey("user.id"), nullable=False)
    session_token: Mapped[str] = mapped_column(Text, unique=True, nullable=False)
    expires_at: Mapped[datetime] = mapped_column(DateTime, nullable=False)

    user: Mapped["User"] = relationship(back_populates="sessions")


class TransactionSource(Base):
    __tablename__ = "transaction_source"

    id: Mapped[int] = mapped_column(Integer, primary_key=True, autoincrement=True)
    name: Mapped[str] = mapped_column(Text, nullable=False)
    user_id: Mapped[int] = mapped_column(ForeignKey("user.id"), nullable=False)
    archived: Mapped[bool] = mapped_column(Boolean, default=False)
    source_kind: Mapped[SourceKind] = mapped_column(
        Enum(SourceKind), default=SourceKind.account
    )

    __table_args__ = (
        UniqueConstraint("user_id", "name", name="uq_transaction_source"),
    )


class Category(Base):
    __tablename__ = "category"

    id: Mapped[int] = mapped_column(Integer, primary_key=True, autoincrement=True)
    name: Mapped[str] = mapped_column(Text, nullable=False)
    source_id: Mapped[int] = mapped_column(
        ForeignKey("transaction_source.id"), nullable=False
    )
    user_id: Mapped[int] = mapped_column(ForeignKey("user.id"), nullable=False)
    archived: Mapped[bool] = mapped_column(Boolean, default=False)

    __table_args__ = (UniqueConstraint("name", "source_id", name="uq_category"),)


class Transaction(Base):
    __tablename__ = "transaction"

    id: Mapped[int] = mapped_column(Integer, primary_key=True, autoincrement=True)
    description: Mapped[str] = mapped_column(Text, nullable=False)
    category_id: Mapped[int] = mapped_column(ForeignKey("category.id"), nullable=False)
    date_of_transaction: Mapped[datetime] = mapped_column(DateTime, nullable=False)
    amount: Mapped[float] = mapped_column(Integer, nullable=False)
    transaction_source_id: Mapped[int] = mapped_column(
        ForeignKey("transaction_source.id"), nullable=False
    )
    kind: Mapped[TransactionKind] = mapped_column(Enum(TransactionKind), nullable=False)
    uploaded_pdf_id: Mapped[Optional[int]] = mapped_column(
        ForeignKey("uploaded_pdf.id"), nullable=True
    )
    user_id: Mapped[int] = mapped_column(ForeignKey("user.id"), nullable=False)
    archived: Mapped[bool] = mapped_column(Boolean, default=False)


class UploadedPdf(Base):
    __tablename__ = "uploaded_pdf"

    id: Mapped[int] = mapped_column(Integer, primary_key=True, autoincrement=True)
    filename: Mapped[str] = mapped_column(Text, nullable=False)
    raw_content: Mapped[str] = mapped_column(Text, nullable=False)
    raw_content_hash: Mapped[str] = mapped_column(Text, nullable=False)
    upload_time: Mapped[str] = mapped_column(
        DateTime, nullable=False
    )  # Consider changing to DateTime
    user_id: Mapped[int] = mapped_column(ForeignKey("user.id"), nullable=False)
    archived: Mapped[bool] = mapped_column(Boolean, default=False)

    __table_args__ = (
        UniqueConstraint("raw_content_hash", "user_id", name="uq_uploaded_pdf"),
    )


class UploadConfiguration(Base):
    __tablename__ = "upload_configuration"

    id: Mapped[int] = mapped_column(Integer, primary_key=True, autoincrement=True)
    filename_regex: Mapped[Optional[str]] = mapped_column(Text, nullable=True)
    start_keyword: Mapped[Optional[str]] = mapped_column(Text, nullable=True)
    end_keyword: Mapped[Optional[str]] = mapped_column(Text, nullable=True)
    transaction_source_id: Mapped[int] = mapped_column(
        ForeignKey("transaction_source.id"), nullable=False
    )
    user_id: Mapped[int] = mapped_column(ForeignKey("user.id"), nullable=False)

    __table_args__ = (
        UniqueConstraint("transaction_source_id", name="uq_upload_configuration"),
    )


class SankeyConfig(Base):
    __tablename__ = "sankey_config"

    id: Mapped[int] = mapped_column(Integer, primary_key=True, autoincrement=True)
    name: Mapped[str] = mapped_column(Text, nullable=False)
    user_id: Mapped[int] = mapped_column(ForeignKey("user.id"), nullable=False)


class ColChartConfig(Base):
    __tablename__ = "col_chart_config"

    id: Mapped[int] = mapped_column(Integer, primary_key=True, autoincrement=True)
    name: Mapped[str] = mapped_column(Text, nullable=False)
    user_id: Mapped[int] = mapped_column(ForeignKey("user.id"), nullable=False)
    active: Mapped[bool] = mapped_column(Boolean, nullable=False)

    __table_args__ = (
        UniqueConstraint("user_id", "active", name="uq_col_chart_config"),
    )


class ProcessFileJob(Base):
    __tablename__ = "process_file_job"

    id: Mapped[int] = mapped_column(Integer, primary_key=True, autoincrement=True)
    created_at: Mapped[datetime] = mapped_column(
        DateTime, default=lambda: datetime.now(timezone.utc)
    )
    last_tried_at: Mapped[Optional[datetime]] = mapped_column(DateTime, nullable=True)
    status: Mapped[JobStatus] = mapped_column(Enum(JobStatus), nullable=False)
    user_id: Mapped[int] = mapped_column(ForeignKey("user.id"), nullable=False)
    config_id: Mapped[Optional[int]] = mapped_column(
        ForeignKey("upload_configuration.id"), nullable=True
    )
    pdf_id: Mapped[int] = mapped_column(ForeignKey("uploaded_pdf.id"), nullable=False)
    archived: Mapped[bool] = mapped_column(Boolean, default=False)
    attempt_count: Mapped[int] = mapped_column(Integer, default=0)

    __table_args__ = (UniqueConstraint("pdf_id", name="uq_process_file_job"),)
