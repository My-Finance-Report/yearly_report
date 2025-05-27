from typing import NewType
from decimal import Decimal
from app.models.models import Base
from sqlalchemy import (
    Boolean,
    ForeignKey,
    Text,
    Numeric,
    Integer,
)
from sqlalchemy.orm import Mapped, mapped_column

from app.models.pos.orders import OrderableId, OrderItemId
from app.models.user import UserId


VariantId = NewType("VariantId", int)
VariantGroupId = NewType("VariantGroupId", int)


class Variant(Base):
    __tablename__ = "variant"

    id: Mapped[VariantId] = mapped_column(Integer, primary_key=True, autoincrement=True)
    name: Mapped[str] = mapped_column(Text, nullable=False)
    price_delta: Mapped[Decimal] = mapped_column(Numeric, nullable=False)
    variant_group_id: Mapped[VariantGroupId] = mapped_column(
        ForeignKey("variant_group.id"), nullable=False
    )
    user_id: Mapped[UserId] = mapped_column(ForeignKey("user.id"), nullable=False)
    active: Mapped[bool] = mapped_column(Boolean, nullable=False)


class VariantGroup(Base):
    __tablename__ = "variant_group"

    id: Mapped[VariantGroupId] = mapped_column(
        Integer, primary_key=True, autoincrement=True
    )
    user_id: Mapped[UserId] = mapped_column(ForeignKey("user.id"), nullable=False)
    name: Mapped[str] = mapped_column(Text, nullable=False)
    required: Mapped[bool] = mapped_column(Boolean, nullable=False)
    active: Mapped[bool] = mapped_column(Boolean, nullable=False, default=True)
    order_of_appearance: Mapped[int] = mapped_column(Integer, nullable=False)


class VariantGroupOrderable(Base):
    __tablename__ = "variant_group_orderable"

    id: Mapped[int] = mapped_column(Integer, primary_key=True, autoincrement=True)
    user_id: Mapped[UserId] = mapped_column(ForeignKey("user.id"), nullable=False)
    variant_group_id: Mapped[VariantGroupId] = mapped_column(
        ForeignKey("variant_group.id"), nullable=False
    )
    orderable_id: Mapped[OrderableId] = mapped_column(
        ForeignKey("orderable.id"), nullable=False
    )


class SelectedVariant(Base):
    __tablename__ = "selected_variant"

    id: Mapped[VariantId] = mapped_column(Integer, primary_key=True, autoincrement=True)
    variant_id: Mapped[VariantId] = mapped_column(
        ForeignKey("variant.id"), nullable=False
    )
    order_item_id: Mapped[OrderItemId] = mapped_column(
        ForeignKey("order_item.id"), nullable=False
    )
    user_id: Mapped[UserId] = mapped_column(ForeignKey("user.id"), nullable=False)
