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
    VariantGroupId,
)
from app.models.user import User

router = APIRouter(prefix="/pos", tags=["pos"])


class VariantBase(BaseModel):
    id: int
    name: str
    priceDelta: Decimal

    class Config:
        from_attributes = True


class VariantGroupBase(BaseModel):
    id: int
    name: str
    required: bool
    order_of_appearance: int
    variants: List[VariantBase]

    class Config:
        from_attributes = True


class OrderableBase(BaseModel):
    id: Optional[int] = None
    name: str
    price: Decimal
    variantGroups: List[VariantGroupBase]

    class Config:
        from_attributes = True


class SelectedVariantBase(BaseModel):
    groupId: str
    id: str
    name: str
    priceDelta: Decimal

    class Config:
        from_attributes = True


class OrderItemBase(BaseModel):
    orderable: OrderableBase
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
@router.get("/menu", response_model=List[OrderableBase])
def get_menu(
    db: Session = Depends(get_db), current_user: User = Depends(get_current_user)
) -> list[OrderableBase]:
    """Get all menu items (orderables) with their variant groups and variants"""
    # Get all orderables
    orderables = db.query(Orderable).filter(Orderable.user_id == current_user.id).all()

    # For each orderable, get its variant groups and variants
    result = []
    for orderable in orderables:
        variant_groups = (
            db.query(VariantGroup, Orderable)
            .filter(
                VariantGroup.orderable_id == orderable.id,
                VariantGroup.user_id == current_user.id,
            )
            .all()
        )

        variant_groups_data = []
        for group, orderable in variant_groups:
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
                VariantGroupBase(
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
            OrderableBase(
                id=orderable.id,
                name=orderable.name,
                price=orderable.price,
                variantGroups=sorted(
                    variant_groups_data, key=lambda x: x.order_of_appearance
                ),
            )
        )

    return result


@router.post("/menu", response_model=OrderableBase)
def create_menu_item(
    item: OrderableBase,
    db: Session = Depends(get_db),
    current_user: User = Depends(get_current_user),
) -> OrderableBase:
    """Create a new menu item with its variant groups and variants"""
    # Create orderable
    orderable = Orderable(name=item.name, price=item.price, user_id=current_user.id)
    db.add(orderable)
    db.flush()  # Get the ID

    # Create variant groups and variants
    for index, group in enumerate(item.variantGroups):
        variant_group = VariantGroup(
            name=group.name,
            required=group.required,
            order_of_appearance=index,
            orderable_id=orderable.id,
            user_id=current_user.id,
            active=True,
        )
        db.add(variant_group)
        db.flush()  # Get the ID

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
    return item


@router.put("/menu/{orderable_id}", response_model=OrderableBase)
def update_menu_item(
    orderable_id: int,
    item: OrderableBase,
    db: Session = Depends(get_db),
    current_user: User = Depends(get_current_user),
) -> OrderableBase:
    """Update a menu item with its variant groups and variants"""
    orderable = db.query(Orderable).filter(Orderable.id == orderable_id).first()
    if not orderable:
        raise HTTPException(status_code=404, detail="Menu item not found")

    orderable.name = item.name
    orderable.price = item.price

    # Get existing groups and create a map by name for easy lookup
    existing_groups = (
        db.query(VariantGroup)
        .filter(
            VariantGroup.orderable_id == orderable_id,
            VariantGroup.user_id == current_user.id,
            VariantGroup.active == True,
        )
        .all()
    )
    existing_groups_by_id: dict[int, VariantGroup] = {g.id: g for g in existing_groups}

    updated_group_ids = set()

    for group in item.variantGroups:
        if group.id in existing_groups_by_id:
            # Update existing group
            variant_group = existing_groups_by_id[group.id]
            variant_group.required = group.required
            variant_group.order_of_appearance = group.order_of_appearance
            updated_group_ids.add(variant_group.id)

            # Deactivate old variants
            db.query(Variant).filter(
                Variant.variant_group_id == variant_group.id,
                Variant.user_id == current_user.id,
            ).update({Variant.active: False})
        else:
            # Create new group
            variant_group = VariantGroup(
                name=group.name,
                required=group.required,
                order_of_appearance=group.order_of_appearance,
                orderable_id=orderable.id,
                user_id=current_user.id,
                active=True,
            )
            db.add(variant_group)
            db.flush()

        # Add new variants
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

    for existing_group in existing_groups:
        if existing_group.id not in updated_group_ids:
            existing_group.active = False

    db.commit()
    return item


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
        db.query(VariantGroup)
        .filter(
            VariantGroup.orderable_id == orderable_id,
            VariantGroup.user_id == current_user.id,
        )
        .all()
    )

    if not orderable:
        raise HTTPException(status_code=404, detail="Menu item not found")

    # Delete associated variant groups and variants
    variant_groups = (
        db.query(VariantGroup)
        .filter(
            VariantGroup.orderable_id == orderable_id,
            VariantGroup.user_id == current_user.id,
        )
        .all()
    )

    for group in variant_groups:
        variants = (
            db.query(Variant)
            .filter(
                Variant.variant_group_id == group.id, Variant.user_id == current_user.id
            )
            .all()
        )
        for variant in variants:
            db.delete(variant)
        db.delete(group)

    db.delete(orderable)
    db.commit()
    return {"message": "Menu item deleted"}


# Order endpoints
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
                    orderable=OrderableBase(
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
