from datetime import datetime

from pydantic import BaseModel


class PlaidLinkTokenRequest(BaseModel):
    """Request model for creating a link token."""

    pass


class PlaidLinkTokenResponse(BaseModel):
    """Response model for a link token."""

    link_token: str


class PlaidExchangeTokenRequest(BaseModel):
    """Request model for exchanging a public token."""

    public_token: str


class PlaidAccountBase(BaseModel):
    """Base model for a Plaid account."""

    plaid_account_id: str
    name: str
    mask: str | None = None
    type: str
    subtype: str | None = None
    current_balance: float | None = None  # Latest fetched balance


class PlaidAccount(PlaidAccountBase):
    """Model for a Plaid account with additional fields."""

    id: int
    user_id: int
    plaid_item_id: str
    access_token: str
    created_at: datetime
    current_balance: float | None = None

    class Config:
        orm_mode = True


class PlaidAccountResponse(PlaidAccountBase):
    """Response model for a Plaid account."""

    id: int
    created_at: datetime
    current_balance: float | None = None

    class Config:
        orm_mode = True


class PlaidAccountCreate(BaseModel):
    """Model for creating a Plaid account."""

    plaid_account_id: str
    plaid_item_id: str
    access_token: str
    name: str
    mask: str | None = None
    type: str
    subtype: str | None = None
