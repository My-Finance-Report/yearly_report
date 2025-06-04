from collections import defaultdict
from datetime import datetime, timedelta, timezone
from decimal import Decimal
from typing import List, Optional
from fastapi import APIRouter, Depends, HTTPException, Query
from pydantic import BaseModel
from sqlalchemy.orm import Session

from app.db import get_db, get_db_for_user, get_guest_db, get_current_user
from app.models.pos.orders import GuestOrderer, Order, OrderItem, Orderable
from app.models.pos.shop import Shop
from app.models.pos.variants import (
    Variant,
    VariantGroup,
    SelectedVariant,
    VariantGroupId,
    VariantGroupOrderable,
)
from app.models.user import User

router = APIRouter(prefix="/pos", tags=["pos"])


class VariantBase(BaseModel):
    id: int | None
    name: str
    price_delta: Decimal

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
    variant_groups: List[VariantGroupOutput]

    class Config:
        from_attributes = True


class OrderableOutput(OrderableBase):
    id: int


class OrderableInput(OrderableBase):
    id: Optional[int] = None


class SelectedVariantBase(BaseModel):
    group_id: int
    id: int
    name: str
    price_delta: Decimal

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


class GuestOrdererInput(BaseModel):
    name: str
    phone: str | None
    email: str | None

class GuestOrderInput(BaseModel):
    slug: str
    pickup_time: datetime
    order_items: List[OrderItemBase]
    guest_orderer: GuestOrdererInput

    class Config:
        from_attributes = True



class Availability(BaseModel):
    open_time: datetime
    close_time: datetime

class ShopOut(BaseModel):
    slug: str
    name: str
    availability: List[Availability]


@router.get("/shop/{slug}", response_model=ShopOut)
def get_shop(
    slug: str,
    db: Session = Depends(get_guest_db)
) -> ShopOut:
    return ShopOut(
        slug=slug,
        name="My Sisters Kitchen",
        availability=[
            Availability(open_time=datetime.now(), close_time=datetime.now() + timedelta(hours=1)),
            Availability(open_time=datetime.now() + timedelta(hours=2), close_time=datetime.now() + timedelta(hours=4)),
        ],
    )

@router.post("/guest-order", response_model=OrderBase)
def create_guest_order(
    order: GuestOrderInput,
    guest_db: Session = Depends(get_guest_db),
) -> OrderBase:
    try:
        shop = guest_db.query(Shop).filter(Shop.slug == order.slug).first()

        if not shop:
            raise HTTPException(status_code=404, detail="Shop not found")

        session = next(get_db_for_user(shop.user_id))

        # Validate pickup time
        now = datetime.now(timezone.utc)
        if not order.pickup_time.tzinfo:
            raise HTTPException(status_code=400, detail="Pickup time must be timezone-aware")
        if order.pickup_time < now:
            raise HTTPException(status_code=400, detail="Pickup time must be in the future")

        # Validate all orderables exist
        orderable_ids = [item.orderable.id for item in order.order_items]
        existing_orderables = (
            session.query(Orderable)
            .filter(Orderable.id.in_(orderable_ids))
            .all()
        )
        if len(existing_orderables) != len(orderable_ids):
            raise HTTPException(status_code=400, detail="One or more orderables not found")

        # Create guest orderer
        guest_orderer = GuestOrderer(
            name=order.guest_orderer.name,
            phone=order.guest_orderer.phone,
            email=order.guest_orderer.email
        )
        session.add(guest_orderer)

        # Create order
        db_order = Order(
            placed_at=now,
            user_id=shop.user_id,
            shop_id=shop.id,
            placed_by=guest_orderer.id,
            active=True,
            pickup_time=order.pickup_time
        )
        session.add(db_order)

        # Create order items and variants
        order_items_data = []
        for item in order.order_items:
            order_item = OrderItem(
                order_id=db_order.id,
                orderable_id=item.orderable.id,
                quantity=item.quantity,
                user_id=shop.user_id,
            )
            session.add(order_item)

            # Validate variants belong to the orderable
            if item.variants:
                variant_ids = [int(v.id) for v in item.variants]
                valid_variants = (
                    session.query(Variant)
                    .join(VariantGroup)
                    .join(VariantGroupOrderable)
                    .filter(
                        Variant.id.in_(variant_ids),
                        VariantGroupOrderable.orderable_id == item.orderable.id
                    )
                    .all()
                )
                if len(valid_variants) != len(variant_ids):
                    raise HTTPException(
                        status_code=400,
                        detail=f"Invalid variants for orderable {item.orderable.id}"
                    )

                for variant in item.variants:
                    session.add(
                        SelectedVariant(
                            variant_id=int(variant.id),
                            order_item_id=order_item.id,
                            user_id=shop.user_id,
                        )
                    )

            order_items_data.append(
                OrderItemBase(
                    orderable=OrderableOutput(
                        id=item.orderable.id,
                        name=item.orderable.name,
                        price=item.orderable.price,
                        variant_groups=item.orderable.variant_groups,
                    ),
                    variants=[
                        SelectedVariantBase(
                            group_id=variant.group_id,
                            id=variant.id,
                            name=variant.name,
                            price_delta=variant.price_delta,
                        )
                        for variant in item.variants
                    ],
                    quantity=item.quantity,
                )
            )

        session.commit()
        return OrderBase(
            id=str(db_order.id),
            timestamp=db_order.placed_at,
            orderItems=order_items_data,
        )

    except HTTPException:
        session.rollback()
        raise
    except Exception as e:
        session.rollback()
        raise HTTPException(status_code=500, detail=str(e))


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
                        VariantBase(id=v.id, name=v.name, price_delta=v.price_delta)
                        for v in variants
                    ],
                )
            )

        result.append(
            OrderableOutput(
                id=orderable.id,
                name=orderable.name,
                price=orderable.price,
                variant_groups=sorted(
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

    for group in item.variant_groups:
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
        variant_groups=item.variant_groups,
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

    for group in item.variant_groups:
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
        variant_groups=item.variant_groups,
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
                price_delta=variant.price_delta,
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
                price_delta=variant.price_delta,
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
                price_delta=variant.price_delta,
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
                    VariantBase(id=v.id, name=v.name, price_delta=v.price_delta)
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
    db_order = Order(
        placed_at=datetime.now(timezone.utc), user_id=current_user.id, active=True
    )
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


def get_variant_group_map(
    db: Session, current_user: User
) -> dict[VariantGroupId, VariantGroupOutput]:
    variant_groups = (
        db.query(VariantGroup).filter(VariantGroup.user_id == current_user.id).all()
    )

    variants = db.query(Variant).filter(Variant.user_id == current_user.id).all()

    variants_lookup = defaultdict(list)
    for variant in variants:
        variants_lookup[variant.variant_group_id].append(
            VariantBase(
                id=variant.id,
                name=variant.name,
                price_delta=variant.price_delta,
            )
        )
    variant_group_map: dict[VariantGroupId, VariantGroupOutput] = {}
    for group in variant_groups:
        variant_group_map[group.id] = VariantGroupOutput(
            id=group.id,
            name=group.name,
            required=group.required,
            order_of_appearance=int(group.order_of_appearance),
            variants=variants_lookup[group.id],
        )
    return variant_group_map


@router.get("/orders", response_model=List[OrderBase])
def get_orders(
    db: Session = Depends(get_db),
    current_user: User = Depends(get_current_user),
    days: int | None = Query(None),
) -> List[OrderBase]:
    """Get all orders for the current user"""
    orders = db.query(Order).filter(
        Order.user_id == current_user.id, Order.active == True
    )

    if days:
        orders = orders.filter(
            Order.placed_at >= datetime.now(timezone.utc) - timedelta(days=days)
        )

    variant_group_map = get_variant_group_map(db, current_user)

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
                db.query(SelectedVariant, Variant)
                .join(Variant, SelectedVariant.variant_id == Variant.id)
                .filter(
                    SelectedVariant.order_item_id == item.id,
                    SelectedVariant.user_id == current_user.id,
                )
                .all()
            )

            variants_data = []
            for selected, variant in selected_variants:
                variants_data.append(
                    SelectedVariantBase(
                        group_id=variant.variant_group_id,
                        id=selected.variant_id,
                        name=variant.name,
                        price_delta=variant.price_delta,
                    )
                )

            order_items_data.append(
                OrderItemBase(
                    orderable=OrderableOutput(
                        id=orderable.id,
                        name=orderable.name,
                        price=orderable.price,
                        variant_groups=[
                            variant_group_map[VariantGroupId(selected_variant_id)]
                            for selected_variant_id in {
                                s.group_id for s in variants_data
                            }
                        ],
                    ),
                    variants=variants_data,
                    quantity=item.quantity,
                )
            )

        result.append(
            OrderBase(
                id=str(order.id),
                timestamp=order.placed_at,
                orderItems=order_items_data,
            )
        )

    return sorted(result, key=lambda x: x.timestamp, reverse=True)
