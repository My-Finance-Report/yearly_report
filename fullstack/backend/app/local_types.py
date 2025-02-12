from app.models import JobStatus, SourceKind, TransactionKind
from pydantic import BaseModel
from datetime import datetime


class Token(BaseModel):
    access_token: str
    token_type: str = "bearer"


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


class UserCreate(UserBase):
    password: str


class UserOut(UserBase):
    id: int
    created_at: None | datetime = None
    updated_at: None | datetime = None


class UserRegister(BaseModel):
    email: str
    password: str
    full_name: str


class UsersPublic(BaseModel):
    pass


class UserUpdate(BaseModel):
    pass


class UserUpdateMe(BaseModel):
    pass


class TransactionSourceBase(BaseModel):
    name: str
    user_id: int
    archived: bool = False
    source_kind: SourceKind = SourceKind.account
    amount: float

    class Config:
        orm_mode = True

class TransactionSourceOut(TransactionSourceBase):
    id: int


class CategoryBase(BaseModel):
    name: str
    source_id: int
    user_id: int
    archived: bool = False

    class Config:
        orm_mode = True

class CategoryOut(CategoryBase):
    id: int


class TransactionBase(BaseModel):
    description: str
    category_id: int
    date_of_transaction: datetime
    amount: float  
    transaction_source_id: int
    kind: TransactionKind 
    uploaded_pdf_id: None|int  = None
    user_id: int
    archived: bool = False

    class Config:
        from_attributes = True 
        orm_mode = True

class TransactionOut(TransactionBase):
    id: int


class UploadedPdfBase(BaseModel):
    filename: str
    raw_content: str
    raw_content_hash: str
    upload_time: datetime  # your model uses DateTime; if it were a string, adjust here.
    user_id: int
    archived: bool = False

    class Config:
        orm_mode = True

class UploadedPdfOut(UploadedPdfBase):
    id: int

class UploadConfigurationBase(BaseModel):
    filename_regex:None |str = None
    start_keyword:None |str = None
    end_keyword: None |str = None
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

class ProcessFileJobBase(BaseModel):
    created_at: datetime
    last_tried_at: None |datetime = None
    status: JobStatus 
    user_id: int
    config_id: None |int = None
    pdf_id: int
    archived: bool = False
    attempt_count: int = 0

    class Config:
        orm_mode = True

class ProcessFileJobOut(ProcessFileJobBase):
    id: int

class TransactionGroup(BaseModel):
    category_id: int
    category_name: str
    total_withdrawals: float
    total_deposits: float
    total_balance: float
    transactions: list[TransactionOut]

class TransactionSourceGroup(BaseModel):
    transaction_source_id: int
    transaction_source_name: str
    total_withdrawals: float
    total_deposits: float
    total_balance: float
    groups: list[TransactionGroup]

    class Config:
        orm_mode = True
        from_attributes = True

class AggregatedTransactions(BaseModel):
    groups: list[TransactionSourceGroup]
    overall_withdrawals: float
    overall_deposits: float
    overall_balance: float