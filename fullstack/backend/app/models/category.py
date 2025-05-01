from typing import NewType
from app.models.models import Base
from sqlalchemy import (
    Boolean,
    ForeignKey,
    Integer,
    Text,
    UniqueConstraint,
)
from sqlalchemy.orm import Mapped, mapped_column
from app.models.transaction_source import TransactionSourceId

from app.models.user import UserId


CategoryId = NewType("CategoryId", int)


class Category(Base):
    __tablename__ = "category"

    id: Mapped[CategoryId] = mapped_column(
        Integer, primary_key=True, autoincrement=True
    )
    name: Mapped[str] = mapped_column(Text, nullable=False)
    source_id: Mapped[TransactionSourceId] = mapped_column(
        ForeignKey("transaction_source.id"), nullable=False
    )
    user_id: Mapped[UserId] = mapped_column(ForeignKey("user.id"), nullable=False)
    archived: Mapped[bool] = mapped_column(Boolean, default=False)

    __table_args__ = (UniqueConstraint("name", "source_id", name="uq_category"),)
