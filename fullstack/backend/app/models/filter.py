import enum
from datetime import datetime, timezone
from typing import  NewType

from pydantic import BaseModel, Field
from app.models.models import Base, JSONType
from sqlalchemy import (
    DateTime,
    ForeignKey,
    String,
    Integer,
)
from sqlalchemy.orm import Mapped, mapped_column
from sqlalchemy import DateTime
from datetime import datetime

from app.models.user import UserId



SavedFilterId = NewType("SavedFilterId", int)

class GroupByOption(str, enum.Enum):
    account = "account"
    category = "category"
    month = "month"
    year = "year"
    budget = "budget"



class FilterEntry(BaseModel):
    value: str


class FilterEntries(BaseModel):
    specifics: list[FilterEntry] | None = None
    visible: bool | None = None
    index: int


class FilterData(BaseModel):
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
    created_at: Mapped[datetime] = mapped_column(
        DateTime, default=lambda: datetime.now(timezone.utc)
    )
    updated_at: Mapped[datetime] = mapped_column(
        DateTime,
        default=lambda: datetime.now(timezone.utc),
        onupdate=lambda: datetime.now(timezone.utc),
    )

