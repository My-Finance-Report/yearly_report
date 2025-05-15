from datetime import datetime, timezone
import enum
from typing import Any, NewType
from app.models.models import Base
from sqlalchemy import JSON, DateTime, ForeignKey, Integer, Enum, String
from sqlalchemy.orm import Mapped, mapped_column
from sqlalchemy import DateTime
from datetime import datetime

from app.models.user import UserId


EffectId = NewType("EffectId", int)


class EffectType(str, enum.Enum):
    EMAIL = "email"
    IN_APP = "in_app"


class EffectConditionals(str, enum.Enum):
    AMOUNT_OVER = "amount_over"
    COUNT_OF_TRANSACTIONS = "count_of_transactions"


class EventType(str, enum.Enum):
    NEW_TRANSACTION = "new_transaction"
    NEW_ACCOUNT_LINKED = "new_account_linked"


class EffectLog(Base):
    __tablename__ = "effect_log"

    id: Mapped[EffectId] = mapped_column(Integer, primary_key=True, autoincrement=True)
    effect_type: Mapped[EffectType] = mapped_column(Enum(EffectType), nullable=False)
    event_type: Mapped[EventType] = mapped_column(Enum(EventType), nullable=False)
    user_id: Mapped[UserId] = mapped_column(ForeignKey("user.id"), nullable=False)
    fired_at: Mapped[datetime] = mapped_column(
        DateTime(timezone=True), default=lambda: datetime.now(timezone.utc)
    )


class Effect(Base):
    __tablename__ = "effect"

    id: Mapped[EffectId] = mapped_column(Integer, primary_key=True, autoincrement=True)
    name: Mapped[str] = mapped_column(String, nullable=False)
    user_id: Mapped[UserId] = mapped_column(ForeignKey("user.id"), nullable=False)
    effect_type: Mapped[EffectType] = mapped_column(Enum(EffectType), nullable=False)
    event_type: Mapped[EventType] = mapped_column(Enum(EventType), nullable=False)
    frequency_days: Mapped[int] = mapped_column(Integer, nullable=False)
    template: Mapped[str] = mapped_column(String, nullable=False)
    subject: Mapped[str] = mapped_column(String, nullable=False)
    condition: Mapped[EffectConditionals] = mapped_column(
        Enum(EffectConditionals), nullable=False
    )
    conditional_parameters: Mapped[dict[str, int]] = mapped_column(
        JSON, nullable=False
    )  # TODO: type this with a dataclass and use JSONType
