from datetime import datetime
from decimal import Decimal
from typing import List, Optional
from fastapi import APIRouter, Depends, HTTPException
from pydantic import BaseModel
from sqlalchemy.orm import Session

from app.db import get_db, get_current_user
from app.models.pos.orders import Order, OrderItem, Orderable
from app.models.pos.variants import (
    Variant,
    VariantGroup,
    SelectedVariant,
    VariantGroupOrderable,
)
from app.models.user import User

router = APIRouter(prefix="/pos", tags=["pos"])


class VariantBase(BaseModel):
    id: int | None
    name: str
    priceDelta: Decimal

    class Config:
        from_attributes = True


class VariantGroupBase(BaseModel):
    name: str
    required: bool
    order_of_appearance: int
    variants: List[VariantBase]

    class Config:
        from_attributes = True


class VariantGroupOutput(VariantGroupBase):
    id: int


class VariantGroupInput(VariantGroupBase):
    id: Optional[int] = None


class OrderableBase(BaseModel):
    name: str
    price: Decimal
    variantGroups: List[VariantGroupOutput]

    class Config:
        from_attributes = True


class OrderableOutput(OrderableBase):
    id: int


class OrderableInput(OrderableBase):
    id: Optional[int] = None


class SelectedVariantBase(BaseModel):
    groupId: str
    id: str
    name: str
    priceDelta: Decimal

    class Config:
        from_attributes = True


class OrderItemBase(BaseModel):
    orderable: OrderableOutput
    variants: List[SelectedVariantBase]
    quantity: int

    class Config:
        from_attributes = True


class OrderBase(BaseModel):
    id: str
    timestamp: datetime
    orderItems: List[OrderItemBase]

    class Config:
        from_attributes = True


# Menu management endpoints
@router.get("/menu", response_model=List[OrderableOutput])
def get_menu(
    db: Session = Depends(get_db), current_user: User = Depends(get_current_user)
) -> list[OrderableOutput]:
    """Get all menu items (orderables) with their variant groups and variants"""
    # Get all orderables
    orderables = db.query(Orderable).filter(Orderable.user_id == current_user.id).all()

    result = []
    for orderable in orderables:
        variant_groups = (
            db.query(VariantGroup)
            .join(
                VariantGroupOrderable,
                VariantGroup.id == VariantGroupOrderable.variant_group_id,
            )
            .filter(
                VariantGroupOrderable.orderable_id == orderable.id,
                VariantGroup.user_id == current_user.id,
            )
            .all()
        )

        variant_groups_data = []
        for group in variant_groups:
            if not group.active:
                continue
            variants = (
                db.query(Variant)
                .filter(
                    Variant.variant_group_id == group.id,
                    Variant.user_id == current_user.id,
                    Variant.active,
                )
                .all()
            )

            variant_groups_data.append(
                VariantGroupOutput(
                    id=group.id,
                    name=group.name,
                    required=group.required,
                    order_of_appearance=int(group.order_of_appearance),
                    variants=[
                        VariantBase(id=v.id, name=v.name, priceDelta=v.price_delta)
                        for v in variants
                    ],
                )
            )

        result.append(
            OrderableOutput(
                id=orderable.id,
                name=orderable.name,
                price=orderable.price,
                variantGroups=sorted(
                    variant_groups_data, key=lambda x: x.order_of_appearance
                ),
            )
        )

    return result


def create_menu_item(
    db: Session, current_user: User, item: OrderableInput
) -> OrderableOutput:
    orderable = Orderable(name=item.name, price=item.price, user_id=current_user.id)
    db.add(orderable)
    db.flush()

    for group in item.variantGroups:
        new = VariantGroupOrderable(
            orderable_id=orderable.id,
            user_id=current_user.id,
            variant_group_id=group.id,
        )

        db.add(new)

    db.commit()
    return OrderableOutput(
        id=orderable.id,
        name=orderable.name,
        price=orderable.price,
        variantGroups=item.variantGroups,
    )


def update_menu_item(
    db: Session, current_user: User, item: OrderableInput
) -> OrderableOutput:
    orderable = db.query(Orderable).filter(Orderable.id == item.id).first()
    if not orderable:
        raise HTTPException(status_code=404, detail="Menu item not found")

    orderable.name = item.name
    orderable.price = item.price

    existing_groups = (
        db.query(VariantGroupOrderable)
        .filter(
            VariantGroupOrderable.orderable_id == orderable.id,
            VariantGroupOrderable.user_id == current_user.id,
        )
        .all()
    )
    existing_groups_by_id: dict[int, VariantGroupOrderable] = {
        g.id: g for g in existing_groups
    }

    updated_group_ids = set()

    for group in item.variantGroups:
        if group.id is None:
            raise HTTPException(status_code=400, detail="Variant group ID is required")
        if group.id in existing_groups_by_id:
            updated_group_ids.add(group.id)
        else:
            new = VariantGroupOrderable(
                orderable_id=orderable.id,
                variant_group_id=group.id,
                user_id=current_user.id,
            )
            db.add(new)
            updated_group_ids.add(group.id)

    for existing_group in existing_groups:
        if existing_group.id not in updated_group_ids:
            db.delete(existing_group)

    db.commit()
    return OrderableOutput(
        id=orderable.id,
        name=orderable.name,
        price=orderable.price,
        variantGroups=item.variantGroups,
    )


@router.post("/menu", response_model=OrderableOutput)
def create_or_update_menu_item(
    item: OrderableInput,
    db: Session = Depends(get_db),
    current_user: User = Depends(get_current_user),
) -> OrderableOutput:
    """Create a new menu item with its variant groups and variants"""
    callable = create_menu_item if item.id is None else update_menu_item

    return callable(db, current_user, item)


@router.delete("/menu/{orderable_id}")
def delete_menu_item(
    orderable_id: int,
    db: Session = Depends(get_db),
    current_user: User = Depends(get_current_user),
) -> dict[str, str]:
    """Delete a menu item"""
    orderable = (
        db.query(Orderable)
        .filter(Orderable.id == orderable_id, Orderable.user_id == current_user.id)
        .first()
    )

    if not orderable:
        raise HTTPException(status_code=404, detail="Menu item not found")

    variant_groups = (
        db.query(VariantGroupOrderable)
        .join(VariantGroup, VariantGroup.id == VariantGroupOrderable.variant_group_id)
        .filter(
            VariantGroupOrderable.orderable_id == orderable_id,
            VariantGroup.user_id == current_user.id,
        )
        .all()
    )

    if not variant_groups:
        raise HTTPException(status_code=404, detail="Menu item not found")

    for variant_group in variant_groups:
        db.delete(variant_group)

    db.delete(orderable)
    db.commit()
    return {"message": "Menu item deleted"}


def create_variant_group(
    db: Session, current_user: User, group: VariantGroupInput
) -> VariantGroupOutput:
    variant_group = VariantGroup(
        name=group.name,
        required=group.required,
        order_of_appearance=group.order_of_appearance,
        user_id=current_user.id,
        active=True,
    )
    db.add(variant_group)
    db.flush()

    for variant in group.variants:
        db.add(
            Variant(
                name=variant.name,
                price_delta=variant.priceDelta,
                variant_group_id=variant_group.id,
                user_id=current_user.id,
                active=True,
            )
        )

    db.commit()
    return VariantGroupOutput(
        id=variant_group.id,
        name=variant_group.name,
        required=variant_group.required,
        order_of_appearance=variant_group.order_of_appearance,
        variants=group.variants,
    )


def update_variant_group(
    db: Session, current_user: User, group: VariantGroupInput
) -> VariantGroupOutput:
    variant_group = (
        db.query(VariantGroup)
        .filter(
            VariantGroup.id == group.id,
            VariantGroup.user_id == current_user.id,
            VariantGroup.active == True,
        )
        .first()
    )
    if not variant_group:
        raise HTTPException(status_code=404, detail="Variant group not found")

    variant_group.name = group.name
    variant_group.required = group.required
    variant_group.order_of_appearance = group.order_of_appearance

    db.query(Variant).filter(
        Variant.variant_group_id == group.id,
        Variant.user_id == current_user.id,
    ).update({Variant.active: False})

    for variant in group.variants:
        db.add(
            Variant(
                name=variant.name,
                price_delta=variant.priceDelta,
                variant_group_id=group.id,
                user_id=current_user.id,
                active=True,
            )
        )

    db.commit()

    return VariantGroupOutput(
        id=variant_group.id,
        name=variant_group.name,
        required=variant_group.required,
        order_of_appearance=variant_group.order_of_appearance,
        variants=[
            VariantBase(
                id=variant.id,
                name=variant.name,
                priceDelta=variant.priceDelta,
            )
            for variant in group.variants
        ],
    )


@router.post("/variant-groups", response_model=VariantGroupOutput)
def create_or_update_variant_group(
    *,
    db: Session = Depends(get_db),
    current_user: User = Depends(get_current_user),
    group: VariantGroupInput,
) -> VariantGroupOutput:
    """Create a new variant group."""
    callable = update_variant_group if group.id else create_variant_group

    return callable(db, current_user, group)


@router.delete("/variant-groups/{group_id}")
def delete_variant_group(
    *,
    db: Session = Depends(get_db),
    current_user: User = Depends(get_current_user),
    group_id: int,
) -> dict[str, bool]:
    """Delete a variant group."""
    # Get variant group
    variant_group = (
        db.query(VariantGroup)
        .filter(
            VariantGroup.id == group_id,
            VariantGroup.user_id == current_user.id,
            VariantGroup.active == True,
        )
        .first()
    )
    if not variant_group:
        raise HTTPException(status_code=404, detail="Variant group not found")

    # Soft delete variant group and its variants
    variant_group.active = False
    db.query(Variant).filter(
        Variant.variant_group_id == group_id,
        Variant.user_id == current_user.id,
    ).update({Variant.active: False})

    db.commit()
    return {"success": True}


@router.get("/variant-groups", response_model=List[VariantGroupOutput])
def get_variant_groups(
    db: Session = Depends(get_db),
    current_user: User = Depends(get_current_user),
) -> List[VariantGroupOutput]:
    """Get all variant groups for reuse"""
    variant_groups = (
        db.query(VariantGroup)
        .filter(
            VariantGroup.user_id == current_user.id,
            VariantGroup.active == True,
        )
        .all()
    )

    result = []
    for group in variant_groups:
        variants = (
            db.query(Variant)
            .filter(
                Variant.variant_group_id == group.id,
                Variant.user_id == current_user.id,
                Variant.active == True,
            )
            .all()
        )

        result.append(
            VariantGroupOutput(
                id=group.id,
                name=group.name,
                required=group.required,
                order_of_appearance=int(group.order_of_appearance),
                variants=[
                    VariantBase(id=v.id, name=v.name, priceDelta=v.price_delta)
                    for v in variants
                ],
            )
        )

    return result


@router.post("/orders", response_model=OrderBase)
def create_order(
    order: OrderBase,
    db: Session = Depends(get_db),
    current_user: User = Depends(get_current_user),
) -> OrderBase:
    """Create a new order with order items and selected variants"""
    # Create order
    db_order = Order(placed_at=order.timestamp, user_id=current_user.id, active=True)
    db.add(db_order)
    db.flush()

    # Create order items and selected variants
    for item in order.orderItems:
        order_item = OrderItem(
            order_id=db_order.id,
            orderable_id=item.orderable.id,
            quantity=item.quantity,
            user_id=current_user.id,
        )
        db.add(order_item)
        db.flush()

        for variant in item.variants:
            db.add(
                SelectedVariant(
                    variant_id=int(variant.id),
                    order_item_id=order_item.id,
                    user_id=current_user.id,
                )
            )

    db.commit()
    return order


@router.get("/orders", response_model=List[OrderBase])
def get_orders(
    db: Session = Depends(get_db), current_user: User = Depends(get_current_user)
) -> List[OrderBase]:
    """Get all orders for the current user"""
    orders = (
        db.query(Order)
        .filter(Order.user_id == current_user.id, Order.active == True)
        .all()
    )

    result = []
    for order in orders:
        order_items = (
            db.query(OrderItem)
            .filter(
                OrderItem.order_id == order.id, OrderItem.user_id == current_user.id
            )
            .all()
        )

        order_items_data = []
        for item in order_items:
            orderable = (
                db.query(Orderable).filter(Orderable.id == item.orderable_id).one()
            )
            selected_variants = (
                db.query(SelectedVariant)
                .filter(
                    SelectedVariant.order_item_id == item.id,
                    SelectedVariant.user_id == current_user.id,
                )
                .all()
            )

            variants_data = []
            for selected in selected_variants:
                variant = (
                    db.query(Variant).filter(Variant.id == selected.variant_id).one()
                )
                variant_group = (
                    db.query(VariantGroup)
                    .filter(VariantGroup.id == variant.variant_group_id)
                    .one()
                )
                variants_data.append(
                    SelectedVariantBase(
                        groupId=str(variant_group.id),
                        id=str(variant.id),
                        name=variant.name,
                        priceDelta=variant.price_delta,
                    )
                )

            order_items_data.append(
                OrderItemBase(
                    orderable=OrderableOutput(
                        id=orderable.id,
                        name=orderable.name,
                        price=orderable.price,
                        variantGroups=[],
                    ),
                    variants=variants_data,
                    quantity=item.quantity,
                )
            )

        result.append(
            OrderBase(
                id=str(order.id), timestamp=order.placed_at, orderItems=order_items_data
            )
        )

    return result
