from datetime import datetime, timezone
import enum
from typing import  NewType
from app.models.models import Base
from sqlalchemy import (
    DateTime,
    Enum,
    String,
    ForeignKey,
    Integer,
)
from sqlalchemy.orm import Mapped, mapped_column
from sqlalchemy import DateTime
from datetime import datetime

from app.models.user import UserId

WorkerStatusId = NewType("WorkerStatusId", int)


class ProcessingState(str, enum.Enum):
    waiting = "waiting"
    preparing_for_parse = "preparing"
    fetching_transactions = "fetching"
    parsing_transactions = "parsing"
    categorizing_transactions = "categorizing"
    failed = "failed"
    completed = "completed"


class WorkerStatus(Base):
    __tablename__ = "worker_status"

    id: Mapped[WorkerStatusId] = mapped_column(Integer, primary_key=True, autoincrement=True)
    batch_id: Mapped[str] = mapped_column(String, nullable=False)
    status: Mapped[ProcessingState] = mapped_column(
        Enum(ProcessingState), nullable=False
    )
    user_id: Mapped[UserId] = mapped_column(ForeignKey("user.id"), nullable=False)
    created_at: Mapped[datetime] = mapped_column(
        DateTime(timezone=True), default=lambda: datetime.now(timezone.utc)
    )
    updated_at: Mapped[datetime] = mapped_column(
        DateTime(timezone=True),
        default=lambda: datetime.now(timezone.utc),
        onupdate=lambda: datetime.now(timezone.utc),
    )
    additional_info: Mapped[str | None] = mapped_column(String, nullable=True)

