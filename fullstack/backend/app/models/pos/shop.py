from typing import NewType
from app.models.models import Base
from app.models.user import UserId
from sqlalchemy import (
    Boolean,
    Integer,
    ForeignKey,
)
from sqlalchemy.orm import Mapped, mapped_column
from sqlalchemy import Boolean, Integer, String

ShopId = NewType("ShopId", int)


class Shop(Base):
    __tablename__ = "shop"

    id: Mapped[ShopId] = mapped_column(Integer, primary_key=True, autoincrement=True)
    user_id: Mapped[UserId] = mapped_column(ForeignKey("user.id"), nullable=False)
    slug: Mapped[str] = mapped_column(String(255), nullable=False)
    name: Mapped[str] = mapped_column(String(255), nullable=False)
    active: Mapped[bool] = mapped_column(Boolean, nullable=False)
