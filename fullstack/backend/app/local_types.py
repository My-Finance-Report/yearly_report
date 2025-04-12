from datetime import datetime
from decimal import Decimal

from pydantic import BaseModel

from app.models import (
    BudgetCategoryLinkId,
    BudgetEntryId,
    BudgetId,
    CategoryBase,
    CategoryId,
    GroupByOption,
    JobStatus,
    PlaidSyncLogId,
    ProcessingState,
    TransactionBase,
    TransactionSourceBase,
    TransactionSourceId,
    UserId,
    UserSettings,
)


class Token(BaseModel):
    access_token: str | None = None
    token_type: str = "bearer"
    requires_2fa: bool = False
    requires_2fa_setup: bool = False
    temp_token: str | None = None


class TokenPayload(BaseModel):
    sub: int | None = None


class Message(BaseModel):
    message: str


# admins i think?
class NewPassword(BaseModel):
    token: Token
    new_password: str


class UserNewPassword(BaseModel):
    new_password: str
    old_password: str


class UserBase(BaseModel):
    full_name: str
    email: str
    is_active: bool = True
    is_superuser: bool = False

    class Config:
        orm_mode = True


class UserOut(UserBase):
    id: int
    created_at: None | datetime = None
    updated_at: None | datetime = None
    settings: UserSettings


class UserRegister(BaseModel):
    email: str
    password: str
    full_name: str
    is_superuser: bool = False
    is_active: bool = True


class UsersPublic(BaseModel):
    data: list[UserOut]
    count: int


class UserUpdate(BaseModel):
    id: int
    password: None | str
    full_name: str
    email: str
    is_superuser: bool = False
    is_active: bool = True


class UserUpdateMe(BaseModel):
    email: str
    settings: UserSettings | None = None


class TransactionSourceOut(TransactionSourceBase):
    id: int
    is_plaid_connected: bool = False


class CategoryOut(CategoryBase):
    id: int
    stylized_name: str


class TransactionOut(TransactionBase):
    id: int


class TransactionEdit(TransactionBase):
    id: CategoryId


class UploadedPdfBase(BaseModel):
    filename: str
    nickname: str | None = None
    raw_content: str
    raw_content_hash: str
    upload_time: datetime
    user_id: int
    archived: bool = False

    class Config:
        orm_mode = True
        from_attributes = True


class UploadConfigurationBase(BaseModel):
    filename_regex: None | str = None
    start_keyword: None | str = None
    end_keyword: None | str = None
    transaction_source_id: int
    user_id: int

    class Config:
        orm_mode = True


class UploadConfigurationOut(UploadConfigurationBase):
    id: int


class SankeyConfigBase(BaseModel):
    name: str
    user_id: int

    class Config:
        orm_mode = True


class SankeyConfigOut(SankeyConfigBase):
    id: int


class ColChartConfigBase(BaseModel):
    name: str
    user_id: int
    active: bool

    class Config:
        orm_mode = True


class ColChartConfigOut(ColChartConfigBase):
    id: int


class BudgetCategoryLinkBase(BaseModel):
    budget_entry_id: BudgetEntryId
    category_id: CategoryId


class BudgetCategoryLinkCreate(BaseModel):
    entry_id: BudgetEntryId
    category_id: CategoryId


class BudgetCategoryLinkOut(BudgetCategoryLinkBase):
    id: BudgetCategoryLinkId
    stylized_name: str


class BudgetEntryCreate(BaseModel):
    amount: float
    name: str


class BudgetEntryBase(BaseModel):
    amount: Decimal
    id: BudgetEntryId
    name: str
    budget_id: BudgetId


class BudgetEntryOut(BudgetEntryBase):
    id: BudgetEntryId
    user_id: UserId
    category_links: list[BudgetCategoryLinkOut]


class BudgetEntryEdit(BudgetEntryBase):
    id: BudgetEntryId
    category_links: list[BudgetCategoryLinkCreate]


class BudgetBase(BaseModel):
    user_id: UserId
    name: str
    active: bool = True

    class Config:
        orm_mode = True


class BudgetCreate(BaseModel):
    name: str


class BudgetOut(BudgetBase):
    id: BudgetId
    entries: list[BudgetEntryOut]


class BudgetCategoryLinkStatus(BudgetCategoryLinkOut):
    transactions: list[TransactionOut]
    total: Decimal


class BudgetEntryStatus(BudgetEntryBase):
    category_links_status: dict[str, BudgetCategoryLinkStatus]
    total: Decimal


class BudgetStatus(BudgetBase):
    entry_status: list[BudgetEntryStatus]
    months_with_entries: list[str]


class ProcessFileJobBase(BaseModel):
    created_at: datetime
    last_tried_at: None | datetime = None
    status: JobStatus
    user_id: int
    config_id: None | int = None
    pdf_id: int
    archived: bool = False
    attempt_count: int = 0

    class Config:
        orm_mode = True
        from_attributes = True


class ProcessFileJobOut(ProcessFileJobBase):
    id: int


class UploadedPdfOut(UploadedPdfBase):
    id: int
    nickname: str | None = None
    job: ProcessFileJobOut | None = None
    transaction_source_id: TransactionSourceId | None = None


class TransactionGroup(BaseModel):
    category_id: int
    category_name: str
    total_withdrawals: float
    total_deposits: float
    total_balance: float
    transactions: list[TransactionOut]


class AggregatedGroup(BaseModel):
    # Use a generic name for the grouping key and value.
    group_id: int | str
    group_name: str
    groupby_kind: GroupByOption | None  # only set on leaves
    total_withdrawals: float
    total_deposits: float
    total_balance: float
    budgeted_total: float
    # For non-leaf groups, these will be populated.
    subgroups: list["AggregatedGroup"] = []
    # For leaf groups, this is a list of transactions.
    transactions: list[TransactionOut] = []

    class Config:
        orm_mode = True
        from_attributes = True


AggregatedGroup.model_rebuild()


class TransactionSourceGroup(BaseModel):
    transaction_source_id: int
    transaction_source_name: str
    total_withdrawals: float
    total_deposits: float
    total_balance: float
    groups: list[AggregatedGroup]

    class Config:
        orm_mode = True
        from_attributes = True


class AggregatedTransactions(BaseModel):
    groups: list[AggregatedGroup]
    overall_withdrawals: float
    group_by_ordering: list[GroupByOption]
    overall_deposits: float
    overall_balance: float
    grouping_options_choices: dict[GroupByOption, list[str]]

    class Config:
        orm_mode = True
        from_attributes = True


class SankeyNode(BaseModel):
    id: int
    name: str


class SankeyLink(BaseModel):
    source: int
    target: int
    value: float


class SankeyData(BaseModel):
    nodes: list[SankeyNode]
    links: list[SankeyLink]


class SankeyInputCreate(BaseModel):
    category_id: int


class SankeyLinkageCreate(BaseModel):
    category_id: int
    target_source_id: int


class SankeySibling(BaseModel):
    category_id: int
    category_name: str
    source_id: int


class PossibleSankeyInput(BaseModel):
    category_id: int
    source_id: int
    source_name: str
    category_name: str
    siblings: list[SankeySibling]


class PossibleSankeyLinkage(BaseModel):
    category_id: int
    category_name: str
    target_source_id: int
    target_source_name: str


class SankeyConfigInfo(BaseModel):
    possible_inputs: list[PossibleSankeyInput]
    possible_links: list[PossibleSankeyLinkage]
    existing_inputs: list[PossibleSankeyInput]
    existing_links: list[PossibleSankeyLinkage]


class SankeyConfigCreatePayload(BaseModel):
    inputs: list[SankeyInputCreate]
    links: list[SankeyLinkageCreate]


class PlaidSyncLogOut(BaseModel):
    id: PlaidSyncLogId
    sync_type: str
    start_date: datetime | None = None
    end_date: datetime | None = None
    added_count: int | None = None
    modified_count: int | None = None
    removed_count: int | None = None
    error_message: str | None = None
    created_at: datetime


class WorkerStatusOut(BaseModel):
    id: int
    batch_id: str
    status: ProcessingState
    created_at: datetime
    updated_at: datetime
    additional_info: str






