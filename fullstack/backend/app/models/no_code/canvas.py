from typing import NewType
from app.models.models import Base
from sqlalchemy import (
    ForeignKey,
    UniqueConstraint,
    String,
    Integer,
)
from sqlalchemy.orm import Mapped, mapped_column

from app.models.user import UserId


CanvasId = NewType("CanvasId", int)


class NoCodeCanvas(Base):
    __tablename__ = "no_code_canvas"

    id: Mapped[CanvasId] = mapped_column(Integer, primary_key=True, autoincrement=True)
    name: Mapped[str] = mapped_column(String, nullable=False)
    user_id: Mapped[UserId] = mapped_column(ForeignKey("user.id"), nullable=False)
    slug: Mapped[str] = mapped_column(String, nullable=False)

    __table_args__ = (UniqueConstraint("user_id", "slug", name="uq_user_slug"),)
