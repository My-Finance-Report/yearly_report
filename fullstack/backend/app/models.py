import enum
from dataclasses import dataclass
from datetime import datetime, timezone
from decimal import Decimal
from typing import Generic, NewType, TypeVar

from pydantic import BaseModel, Field
from sqlalchemy import (
    JSON,
    Boolean,
    DateTime,
    Dialect,
    Enum,
    ForeignKey,
    Integer,
    Numeric,
    String,
    Text,
    TypeDecorator,
    UniqueConstraint,
)
from sqlalchemy.orm import DeclarativeBase, Mapped, mapped_column, relationship

T = TypeVar("T", bound=BaseModel)  # This represents any dataclass type


class JSONType(TypeDecorator[T], Generic[T]):
    """
    A generic SQLAlchemy column type that stores any dataclass as JSON.
    """

    impl = JSON

    def __init__(self, dataclass_type: type[T]) -> None:
        """Initialize with the dataclass type that should be used for deserialization."""
        super().__init__()
        self.dataclass_type = dataclass_type

    def process_bind_param(self, value: T | None, _dialect: Dialect) -> str | None:
        """Convert a BaseModel into JSON when writing to the database."""
        if value is not None:
            return value.model_dump_json()
        return None

    def process_result_value(
        self, value: dict[str, str] | str | bytes | bytearray | None, _dialect: Dialect
    ) -> T | None:
        """Convert JSON from the database back into the correct dataclass."""
        if value is None:
            return None

        if isinstance(value, dict):
            # this is a failover from previous json handling
            return self.dataclass_type.model_validate(value)
        else:
            # If we have a string/bytes, use model_validate_json
            return self.dataclass_type.model_validate_json(value)


class Base(DeclarativeBase):
    __rls_enabled__ = True


class GroupByOption(str, enum.Enum):
    account = "account"
    category = "category"
    month = "month"
    year = "year"
    budget = "budget"


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
    plaid_recategorize = "plaid_recategorize"


UserId = NewType("UserId", int)
TransactionId = NewType("TransactionId", int)
PlaidTransactionId = NewType("PlaidTransactionId", str)
CategoryId = NewType("CategoryId", int)
TransactionSourceId = NewType("TransactionSourceId", int)
UploadConfigurationId = NewType("UploadConfigurationId", int)
WorkerJobId = NewType("WorkerJobId", int)
UploadedPdfId = NewType("UploadedPdfId", int)
ColChartConfigId = NewType("ColChartConfigId", int)
BudgetId = NewType("BudgetId", int)
BudgetCategoryLinkId = NewType("BudgetCategoryLinkId", int)
BudgetEntryId = NewType("BudgetEntryId", int)
AuditLogId = NewType("AuditLogId", int)
TransactionReportId = NewType("TransactionReportId", int)
PlaidItemId = NewType("PlaidItemId", int)
PlaidAccountId = NewType("PlaidAccountId", int)
SubscriptionId = NewType("SubscriptionId", int)
PriceId = NewType("PriceId", int)
PlaidSyncLogId = NewType("PlaidSyncLogId", int)
SavedFilterId = NewType("SavedFilterId", int)


class UserSettings(BaseModel):
    has_budget: bool = False
    power_user_filters: bool = False


class User(Base):
    __tablename__ = "user"

    id: Mapped[UserId] = mapped_column(Integer, primary_key=True, autoincrement=True)
    email: Mapped[str] = mapped_column(String, unique=True, nullable=False)
    hashed_password: Mapped[str | None] = mapped_column(String, nullable=True)
    full_name: Mapped[str | None] = mapped_column(String, nullable=True)
    is_active: Mapped[bool] = mapped_column(Boolean, default=True)
    send_email: Mapped[bool] = mapped_column(Boolean, default=True)
    is_superuser: Mapped[bool] = mapped_column(Boolean, default=False)
    created_at: Mapped[datetime] = mapped_column(
        DateTime, default=lambda: datetime.now(timezone.utc)
    )
    onboarding_step: Mapped[int | None] = mapped_column(Integer, nullable=True)
    settings: Mapped[UserSettings] = mapped_column(
        JSONType(UserSettings), nullable=False
    )
    requires_two_factor: Mapped[bool] = mapped_column(Boolean, default=True)
    totp_secret: Mapped[str | None] = mapped_column(String, nullable=True)
    totp_enabled: Mapped[bool] = mapped_column(Boolean, default=False)
    sessions: Mapped[list["UserSession"]] = relationship(back_populates="user")
    oauth_provider: Mapped[str | None] = mapped_column(String, nullable=True)
    oauth_id: Mapped[str | None] = mapped_column(String, nullable=True)
    oauth_access_token: Mapped[str | None] = mapped_column(String, nullable=True)
    oauth_refresh_token: Mapped[str | None] = mapped_column(String, nullable=True)
    oauth_token_expires_at: Mapped[datetime | None] = mapped_column(
        DateTime, nullable=True
    )
    subscription: Mapped["Subscription"] = relationship(
        "Subscription", back_populates="user", uselist=False
    )
    saved_filters: Mapped[list["SavedFilter"]] = relationship(
        "SavedFilter", back_populates="user"
    )


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
    plaid_account_id: Mapped[PlaidAccountId | None] = mapped_column(
        ForeignKey("plaid_account.id"), nullable=True
    )

    # Relationships
    plaid_account: Mapped["PlaidAccount | None"] = relationship(
        "PlaidAccount", back_populates="transaction_source"
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
    external_id: Mapped[PlaidTransactionId | None] = mapped_column(
        String, nullable=True, index=True
    )
    last_updated: Mapped[datetime | None] = mapped_column(
        DateTime, nullable=True, default=lambda: datetime.now(timezone.utc)
    )


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


class WorkerJob(Base):
    __tablename__ = "process_file_job"

    id: Mapped[WorkerJobId] = mapped_column(
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
    pdf_id: Mapped[UploadedPdfId | None] = mapped_column(
        ForeignKey("uploaded_pdf.id"), nullable=True
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
    change: Mapped[AuditChange] = mapped_column(JSONType(AuditChange), nullable=False)
    apply_to_future: Mapped[bool] = mapped_column(Boolean, nullable=False)
    transaction_id: Mapped[TransactionId] = mapped_column(
        ForeignKey("transaction.id"), nullable=False
    )
    details: Mapped[str | None] = mapped_column(Text, nullable=True)
    created_at: Mapped[datetime] = mapped_column(
        DateTime, default=lambda: datetime.now(timezone.utc)
    )


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


@dataclass
class ReportData(BaseModel):
    transactions: list[TransactionBase]
    transaction_sources: list[TransactionSourceBase]
    categories: list[CategoryBase]


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

    accounts: Mapped[list["PlaidAccount"]] = relationship(back_populates="item")


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
    created_at: Mapped[datetime] = mapped_column(
        DateTime, default=lambda: datetime.now(timezone.utc)
    )

    # Relationships
    item: Mapped["PlaidItem"] = relationship(back_populates="accounts")
    transaction_source: Mapped["TransactionSource"] = relationship(
        "TransactionSource", uselist=False, back_populates="plaid_account"
    )


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
    created_at: Mapped[datetime] = mapped_column(
        DateTime, default=lambda: datetime.now(timezone.utc)
    )


class SubscriptionStatus(str, enum.Enum):
    active = "active"
    canceled = "canceled"
    incomplete = "incomplete"
    incomplete_expired = "incomplete_expired"
    past_due = "past_due"
    trialing = "trialing"
    unpaid = "unpaid"


class SubscriptionTier(str, enum.Enum):
    free = "free"
    premium = "premium"
    business = "business"


class Price(Base):
    __tablename__ = "price"

    id: Mapped[PriceId] = mapped_column(Integer, primary_key=True, autoincrement=True)
    stripe_price_id: Mapped[str] = mapped_column(String, nullable=False, unique=True)
    product_name: Mapped[str] = mapped_column(String, nullable=False)
    tier: Mapped[SubscriptionTier] = mapped_column(
        Enum(SubscriptionTier), nullable=False
    )
    unit_amount: Mapped[int] = mapped_column(Integer, nullable=False)  # In cents
    currency: Mapped[str] = mapped_column(String, nullable=False, default="usd")
    recurring_interval: Mapped[str] = mapped_column(
        String, nullable=False
    )  # month, year, etc.
    active: Mapped[bool] = mapped_column(Boolean, nullable=False, default=True)
    created_at: Mapped[datetime] = mapped_column(
        DateTime, default=lambda: datetime.now(timezone.utc)
    )
    max_sources: Mapped[int] = mapped_column(Integer, nullable=False, default=1)
    description: Mapped[str | None] = mapped_column(Text, nullable=True)


class Subscription(Base):
    __tablename__ = "subscription"

    id: Mapped[SubscriptionId] = mapped_column(
        Integer, primary_key=True, autoincrement=True
    )
    user_id: Mapped[UserId] = mapped_column(ForeignKey("user.id"), nullable=False)
    stripe_subscription_id: Mapped[str | None] = mapped_column(
        String, nullable=True, unique=True
    )
    stripe_customer_id: Mapped[str | None] = mapped_column(String, nullable=True)
    price_id: Mapped[PriceId | None] = mapped_column(
        ForeignKey("price.id"), nullable=True
    )
    status: Mapped[SubscriptionStatus] = mapped_column(
        Enum(SubscriptionStatus), nullable=False, default=SubscriptionStatus.active
    )
    current_period_start: Mapped[datetime | None] = mapped_column(
        DateTime, nullable=True
    )
    current_period_end: Mapped[datetime | None] = mapped_column(DateTime, nullable=True)
    cancel_at_period_end: Mapped[bool] = mapped_column(
        Boolean, nullable=False, default=False
    )
    created_at: Mapped[datetime] = mapped_column(
        DateTime, default=lambda: datetime.now(timezone.utc)
    )
    updated_at: Mapped[datetime] = mapped_column(
        DateTime,
        default=lambda: datetime.now(timezone.utc),
        onupdate=lambda: datetime.now(timezone.utc),
    )

    # Relationships
    price: Mapped["Price | None"] = relationship("Price")
    user: Mapped["User"] = relationship("User", back_populates="subscription")


class FilterEntry(BaseModel):
    value: str


class FilterEntries(BaseModel):
    specifics: list[FilterEntry] | None = None
    visible: bool | None = None
    index: int


class FilterData(BaseModel):
    is_default: bool = True
    lookup: dict[GroupByOption, FilterEntries] = Field(
        default_factory=lambda: {
            GroupByOption.category: FilterEntries(
                specifics=None, visible=True, index=0
            ),
            GroupByOption.month: FilterEntries(visible=True, specifics=None, index=1),
            GroupByOption.account: FilterEntries(visible=True, specifics=None, index=2),
        }
    )


class SavedFilter(Base):
    """Model for saved filter configurations."""

    __tablename__ = "saved_filter"

    id: Mapped[SavedFilterId] = mapped_column(
        Integer, primary_key=True, autoincrement=True
    )
    name: Mapped[str] = mapped_column(String, nullable=False)
    description: Mapped[str | None] = mapped_column(String, nullable=True)
    filter_data: Mapped[FilterData] = mapped_column(
        JSONType(FilterData), nullable=False
    )
    user_id: Mapped[UserId] = mapped_column(ForeignKey("user.id"), nullable=False)
    user: Mapped["User"] = relationship("User", back_populates="saved_filters")
    created_at: Mapped[datetime] = mapped_column(
        DateTime, default=lambda: datetime.now(timezone.utc)
    )
    updated_at: Mapped[datetime] = mapped_column(
        DateTime,
        default=lambda: datetime.now(timezone.utc),
        onupdate=lambda: datetime.now(timezone.utc),
    )


class ProcessingState(str, enum.Enum):
    waiting = "waiting"
    preparing_for_parse = "preparing"
    fetching_transactions = "fetching"
    parsing_transactions = "parsing"
    categorizing_transactions = "categorizing"
    failed = "failed"
    completed = "completed"


class WorkerStatus(Base):
    __tablename__ = "worker_status"

    id: Mapped[int] = mapped_column(Integer, primary_key=True, autoincrement=True)
    batch_id: Mapped[str] = mapped_column(String, nullable=False)
    status: Mapped[ProcessingState] = mapped_column(
        Enum(ProcessingState), nullable=False
    )
    user_id: Mapped[UserId] = mapped_column(ForeignKey("user.id"), nullable=False)
    created_at: Mapped[datetime] = mapped_column(
        DateTime(timezone=True), default=lambda: datetime.now(timezone.utc)
    )
    updated_at: Mapped[datetime] = mapped_column(
        DateTime(timezone=True),
        default=lambda: datetime.now(timezone.utc),
        onupdate=lambda: datetime.now(timezone.utc),
    )
    additional_info: Mapped[str | None] = mapped_column(String, nullable=True)
