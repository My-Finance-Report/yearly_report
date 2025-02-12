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
