from datetime import datetime
from typing import Optional
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
    mask: Optional[str] = None
    type: str
    subtype: Optional[str] = None

class PlaidAccount(PlaidAccountBase):
    """Model for a Plaid account with additional fields."""
    id: int
    user_id: int
    plaid_item_id: str
    access_token: str
    created_at: datetime
    
    class Config:
        orm_mode = True

class PlaidAccountResponse(PlaidAccountBase):
    """Response model for a Plaid account."""
    id: int
    created_at: datetime
    
    class Config:
        orm_mode = True

class PlaidAccountCreate(BaseModel):
    """Model for creating a Plaid account."""
    plaid_account_id: str
    plaid_item_id: str
    access_token: str
    name: str
    mask: Optional[str] = None
    type: str
    subtype: Optional[str] = None
