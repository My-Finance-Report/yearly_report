from datetime import datetime

from pydantic import BaseModel

from app.models.stripe import SubscriptionStatus, SubscriptionTier


class SubscriptionDetails(BaseModel):
    tier: SubscriptionTier
    status: SubscriptionStatus
    current_period_end: datetime | None = None
    cancel_at_period_end: bool = False
    max_sources: int
    current_sources: int

    class Config:
        from_attributes = True


class SubscriptionLimits(BaseModel):
    has_reached_limit: bool
    current_count: int
    max_allowed: int


class PriceDetails(BaseModel):
    id: int
    name: str
    description: str | None = None
    price: float
    currency: str
    interval: str
    tier: SubscriptionTier
    max_sources: int

    class Config:
        from_attributes = True


class CheckoutSession(BaseModel):
    checkout_url: str
