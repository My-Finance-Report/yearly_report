from datetime import datetime, timezone
from typing import NewType
from app.models.models import Base, JSONType
from pydantic import BaseModel
from sqlalchemy import (
    Boolean,
    DateTime,
    Integer,
    String,
)
from sqlalchemy.orm import Mapped, mapped_column
from sqlalchemy import DateTime
from datetime import datetime


UserId = NewType("UserId", int)


class UserSettings(BaseModel):
    has_budget: bool = False
    power_user_filters: bool = False
    point_of_sales_user: bool = False

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
    last_visited_at: Mapped[datetime | None] = mapped_column(DateTime, nullable=True)
    requires_two_factor: Mapped[bool] = mapped_column(Boolean, default=True)
    totp_secret: Mapped[str | None] = mapped_column(String, nullable=True)
    totp_enabled: Mapped[bool] = mapped_column(Boolean, default=False)
    oauth_provider: Mapped[str | None] = mapped_column(String, nullable=True)
    oauth_id: Mapped[str | None] = mapped_column(String, nullable=True)
    oauth_access_token: Mapped[str | None] = mapped_column(String, nullable=True)
    oauth_refresh_token: Mapped[str | None] = mapped_column(String, nullable=True)
    oauth_token_expires_at: Mapped[datetime | None] = mapped_column(
        DateTime, nullable=True
    )
