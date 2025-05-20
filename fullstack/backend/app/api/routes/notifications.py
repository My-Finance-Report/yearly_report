from fastapi import APIRouter, Depends, HTTPException, status
from typing import Any, Optional
from datetime import datetime, timedelta

from app.db import Session, get_current_user, get_db
from app.email.send import Email
from app.local_types import EffectOut
from app.models.effect import (
    Effect as EffectModel,
    EffectConditionals,
    EffectType,
    EventType,
    EffectId,
)
from app.models.user import User
from app.models.transaction import TransactionKind
from app.no_code.notifications.effect_generators.seed_effects import (
    new_transaction_effect,
)
from app.no_code.notifications.effects import EffectConfig, Effect
from app.no_code.notifications.events import NewTransactionsEvent
from app.no_code.notifications.trigger import perform_template_replacement
from app.schemas.no_code import NoCodeTransaction
from pydantic import BaseModel

router = APIRouter(prefix="/notification", tags=["no_code"])


class EffectCreate(BaseModel):
    """Schema for creating a new notification effect"""

    name: str
    effect_type: EffectType
    event_type: EventType
    frequency_days: int
    template: str
    subject: str
    condition: EffectConditionals
    conditional_parameters: dict[str, int]


class EffectUpdate(BaseModel):
    """Schema for updating an existing notification effect"""

    name: Optional[str] = None
    effect_type: Optional[EffectType] = None
    event_type: Optional[EventType] = None
    frequency_days: Optional[int] = None
    template: Optional[str] = None
    subject: Optional[str] = None
    condition: Optional[EffectConditionals] = None
    conditional_parameters: Optional[dict[str, int]] = None


@router.get("/effects", response_model=list[EffectOut])
def get_effects(
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> list[EffectOut]:
    """Get all notification effects for the current user"""
    # Query the database for user's effects
    db_effects = session.query(EffectModel).filter(EffectModel.user_id == user.id).all()

    # If no effects exist yet, create a default one
    if not db_effects:
        transaction_effect = new_transaction_effect(session, user)
        # Create a new effect in the database
        db_effect = EffectModel(
            name="New Transactions",
            user_id=user.id,
            effect_type=transaction_effect.type,
            event_type=EventType.NEW_TRANSACTION,
            frequency_days=transaction_effect.config.frequency_days,
            template=transaction_effect.config.template,
            subject=transaction_effect.config.subject,
            condition=transaction_effect.condition,
            conditional_parameters=transaction_effect.conditional_parameters,
        )
        session.add(db_effect)
        session.commit()
        session.refresh(db_effect)
        db_effects = [db_effect]

    # Convert DB models to EffectOut schema
    return [
        EffectOut(
            id=effect.id,
            name=effect.name,
            effect_type=effect.effect_type,
            event_type=effect.event_type,
            config=EffectConfig(
                frequency_days=effect.frequency_days,
                template=effect.template,
                subject=effect.subject,
            ),
            condition=effect.condition,
            conditional_parameters=effect.conditional_parameters,
        )
        for effect in db_effects
    ]


class NotificationPreviewResponse(BaseModel):
    """Response model for notification preview"""

    html: str
    subject: str


@router.get("/preview", response_model=Email)
def preview_notification(
    effect_type: EffectType = EffectType.EMAIL,
    event_type: EventType = EventType.NEW_TRANSACTION,
    template: Optional[str] = None,
    subject: Optional[str] = None,
    num_transactions: int = 3,
    account_name: str = "Demo Account",
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> Email:
    """Preview a notification with sample data"""
    # Create sample transactions
    sample_transactions = []
    for i in range(num_transactions):
        # Create varied transaction amounts
        amount = 100 * (i + 1)
        # Alternate between withdrawals and deposits
        kind = TransactionKind.withdrawal if i % 2 == 0 else TransactionKind.deposit
        # Create transactions on different days
        date = datetime.now() - timedelta(days=i)

        sample_transactions.append(
            NoCodeTransaction(
                id=i,
                category_id=i,
                amount=amount,
                date_of_transaction=date,
                description=f"Sample Transaction {i + 1}",
                kind=kind,
                category_name=f"Category {(i % 3) + 1}",
                account_name=account_name,
            )
        )

    # Create the event
    event = NewTransactionsEvent(
        transactions=sample_transactions,
        account_name=account_name,
        count=len(sample_transactions),
    )

    # Use provided template and subject or defaults
    config = EffectConfig(
        frequency_days=1,
        template=template
        or "Hey there! You have {{ count }} new transaction(s) in My Financé!\n{{ transactions_table }}\n{{ alter_settings }}",
        subject=subject or "New Transactions in My Financé from {{ account_name }}",
    )

    # Create the effect
    effect = Effect(
        type=effect_type,
        config=config,
        condition=EffectConditionals.COUNT_OF_TRANSACTIONS,
        conditional_parameters={"count": 0},
    )

    # Generate the email preview
    return perform_template_replacement(event, effect)


@router.post("/effects", response_model=EffectOut, status_code=status.HTTP_201_CREATED)
def create_effect(
    effect_data: EffectCreate,
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> EffectOut:
    """Create a new notification effect"""
    # Create a new effect in the database
    db_effect = EffectModel(
        name=effect_data.name,
        user_id=user.id,
        effect_type=effect_data.effect_type,
        event_type=effect_data.event_type,
        frequency_days=effect_data.frequency_days,
        template=effect_data.template,
        subject=effect_data.subject,
        condition=effect_data.condition,
        conditional_parameters=effect_data.conditional_parameters,
    )
    session.add(db_effect)
    session.commit()
    session.refresh(db_effect)

    # Return the created effect
    return EffectOut(
        name=db_effect.name,
        effect_type=db_effect.effect_type,
        event_type=db_effect.event_type,
        config=EffectConfig(
            frequency_days=db_effect.frequency_days,
            template=db_effect.template,
            subject=db_effect.subject,
        ),
        condition=db_effect.condition,
        conditional_parameters=db_effect.conditional_parameters,
    )


@router.put("/effects/{effect_id}", response_model=EffectOut)
def update_effect(
    effect_id: int,
    effect_data: EffectUpdate,
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> EffectOut:
    """Update an existing notification effect"""
    # Find the effect
    db_effect = (
        session.query(EffectModel)
        .filter(EffectModel.id == effect_id, EffectModel.user_id == user.id)
        .first()
    )

    if not db_effect:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail="Notification effect not found",
        )

    # Update the effect with the provided data
    update_data = effect_data.dict(exclude_unset=True)
    for key, value in update_data.items():
        setattr(db_effect, key, value)

    session.commit()
    session.refresh(db_effect)

    # Return the updated effect
    return EffectOut(
        id=db_effect.id,
        name=db_effect.name,
        effect_type=db_effect.effect_type,
        event_type=db_effect.event_type,
        config=EffectConfig(
            frequency_days=db_effect.frequency_days,
            template=db_effect.template,
            subject=db_effect.subject,
        ),
        condition=db_effect.condition,
        conditional_parameters=db_effect.conditional_parameters,
    )


@router.delete("/effects/{effect_id}", status_code=status.HTTP_204_NO_CONTENT)
def delete_effect(
    effect_id: int,
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> None:
    """Delete a notification effect"""
    # Find the effect
    db_effect = (
        session.query(EffectModel)
        .filter(EffectModel.id == effect_id, EffectModel.user_id == user.id)
        .first()
    )

    if not db_effect:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail="Notification effect not found",
        )

    # Delete the effect
    session.delete(db_effect)
    session.commit()

    return None
