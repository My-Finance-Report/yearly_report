from datetime import datetime, timezone
import enum
from typing import NewType
from app.models.models import Base
from sqlalchemy import (
    Boolean,
    Enum,
    DateTime,
    ForeignKey,
    Text,
    Integer,
    String,
)
from sqlalchemy.orm import Mapped, mapped_column
from sqlalchemy import DateTime
from datetime import datetime

from app.models.user import UserId


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


SubscriptionId = NewType("SubscriptionId", int)
PriceId = NewType("PriceId", int)


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
