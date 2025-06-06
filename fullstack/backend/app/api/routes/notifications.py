from fastapi import APIRouter, Depends, HTTPException, status
from decimal import Decimal
from typing import Optional
from datetime import datetime, timedelta

from app.db import Session, get_current_user, get_db
from app.email.send import Email
from app.local_types import EffectMappings, EffectOut
from app.models.effect import (
    ConditionalParameters,
    Effect as EffectModel,
    EffectConditionals,
    EffectType,
    EventType,
)
from app.models.user import User
from app.models.transaction import TransactionKind
from app.no_code.notifications.effects import EffectConfig, Effect
from app.no_code.notifications.events import (
    AccountDeactivatedEvent,
    BudgetThresholdExceededEvent,
    NewAccountLinkedEvent,
    NewTransactionsEvent,
)
from app.no_code.notifications.trigger import perform_template_replacement
from app.schemas.no_code import MonthlyTotal, MonthlyTarget,NoCodeBudgetEntry, NoCodeTransaction
from pydantic import BaseModel

router = APIRouter(prefix="/notification", tags=["no_code"])


class EffectCreate(BaseModel):
    """Schema for creating a new notification effect"""

    name: str
    effect_type: EffectType
    active: bool
    event_type: EventType
    frequency_days: int
    template: str
    subject: str
    condition: EffectConditionals
    conditional_parameters: ConditionalParameters


class EffectUpdate(BaseModel):
    """Schema for updating an existing notification effect"""

    name: str
    effect_type: EffectType
    event_type: EventType
    frequency_days: int
    template: str
    subject: str
    condition: EffectConditionals
    conditional_parameters: ConditionalParameters


@router.get("/effects", response_model=list[EffectOut])
def get_effects(
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> list[EffectOut]:
    """Get all notification effects for the current user"""
    # Query the database for user's effects
    db_effects = session.query(EffectModel).filter(EffectModel.user_id == user.id).all()

    # Convert DB models to EffectOut schema
    return [
        EffectOut(
            id=effect.id,
            name=effect.name,
            active=effect.active,
            editable=effect.editable,
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


# TODO express the types without this union somehow
AnyEvent = (
    NewTransactionsEvent
    | NewAccountLinkedEvent
    | AccountDeactivatedEvent
    | BudgetThresholdExceededEvent
)


def get_sample_event(event_type: EventType) -> AnyEvent:
    num_transactions = 3
    account_name = "Demo Account"
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
    if event_type == EventType.NEW_TRANSACTION:
        return NewTransactionsEvent(
            type=EventType.NEW_TRANSACTION,
            transactions=sample_transactions,
            account_name=account_name,
            count=len(sample_transactions),
        )
    elif event_type == EventType.NEW_ACCOUNT_LINKED:
        return NewAccountLinkedEvent(
            type=EventType.NEW_ACCOUNT_LINKED,
            account_name=account_name,
        )
    elif event_type == EventType.BUDGET_THRESHOLD_EXCEEDED:
        return BudgetThresholdExceededEvent(
            type=EventType.BUDGET_THRESHOLD_EXCEEDED,
            transactions=sample_transactions,
            budget_entries=[
                NoCodeBudgetEntry(
                    id=-1,
                    category_name="Category 1",
                    monthly_target=MonthlyTarget(Decimal(100)),
                    current_monthly_total=MonthlyTotal(Decimal(200)),
                )
            ],
        )
    elif event_type == EventType.ACCOUNT_DEACTIVATED:
        return AccountDeactivatedEvent(
            type=EventType.ACCOUNT_DEACTIVATED,
            account_name=account_name,
        )


def get_sample_conditional_parameters(event_type: EventType) -> ConditionalParameters:
    if event_type == EventType.NEW_TRANSACTION:
        return ConditionalParameters(count=0)
    elif event_type == EventType.NEW_ACCOUNT_LINKED:
        return ConditionalParameters()
    elif event_type == EventType.ACCOUNT_DEACTIVATED:
        return ConditionalParameters()
    elif event_type == EventType.BUDGET_THRESHOLD_EXCEEDED:
        return ConditionalParameters(amount=100)


def get_sample_condition(event_type: EventType) -> EffectConditionals:
    if event_type == EventType.NEW_TRANSACTION:
        return EffectConditionals.COUNT_OF_TRANSACTIONS
    elif event_type == EventType.NEW_ACCOUNT_LINKED:
        return EffectConditionals.UNCONDITIONAL
    elif event_type == EventType.ACCOUNT_DEACTIVATED:
        return EffectConditionals.UNCONDITIONAL
    elif event_type == EventType.BUDGET_THRESHOLD_EXCEEDED:
        return EffectConditionals.AMOUNT_OVER


@router.get("/preview", response_model=Email)
def preview_notification(
    effect_type: EffectType = EffectType.EMAIL,
    event_type: EventType = EventType.NEW_TRANSACTION,
    template: Optional[str] = None,
    subject: Optional[str] = None,
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> Email:
    print(template)

    event = get_sample_event(event_type)

    config = EffectConfig(
        frequency_days=1,
        template=template or "",
        subject=subject or "",
    )

    effect = Effect(
        type=effect_type,
        active=True,
        editable=True,
        config=config,
        condition=get_sample_condition(event_type),
        conditional_parameters=get_sample_conditional_parameters(event_type),
    )

    return perform_template_replacement(event, effect)


def to_snake_case(name: str) -> str:
    return "_".join(name.split(" ")).lower()


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
        ref_name=to_snake_case(effect_data.name),
        user_id=user.id,
        active=effect_data.active,
        editable=True,
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

    return EffectOut(
        id=db_effect.id,
        name=db_effect.name,
        effect_type=db_effect.effect_type,
        event_type=db_effect.event_type,
        active=db_effect.active,
        editable=db_effect.editable,
        config=EffectConfig(
            frequency_days=db_effect.frequency_days,
            template=db_effect.template,
            subject=db_effect.subject,
        ),
        condition=db_effect.condition,
        conditional_parameters=db_effect.conditional_parameters,
    )


@router.get("/effect_mappings", response_model=EffectMappings)
def get_effect_mappings(
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> EffectMappings:
    return EffectMappings(
        variables={
            EventType.NEW_TRANSACTION: [
                "transactions_table",
                "count",
                "account_name",
                "alter_settings",
            ],
            EventType.NEW_ACCOUNT_LINKED: [
                "account_name",
                "alter_settings",
            ],
            EventType.ACCOUNT_DEACTIVATED: [
                "account_name",
                "alter_settings",
            ],
            EventType.BUDGET_THRESHOLD_EXCEEDED: [
                "transactions_table",
                "budget_table",
                "alter_settings",
            ],
        },
        allowed_conditional_parameters={
            EventType.NEW_TRANSACTION: [
                EffectConditionals.AMOUNT_OVER,
                EffectConditionals.COUNT_OF_TRANSACTIONS,
            ],
            EventType.NEW_ACCOUNT_LINKED: [],
            EventType.ACCOUNT_DEACTIVATED: [],
            EventType.BUDGET_THRESHOLD_EXCEEDED: [
                EffectConditionals.AMOUNT_OVER,
            ],
        },
    )


@router.put("/toggle-effect/{effect_id}", response_model=EffectOut)
def togge_effect_activity(
    effect_id: int,
    is_active: bool,
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

    db_effect.active = is_active
    session.commit()
    session.refresh(db_effect)

    # Return the updated effect
    return EffectOut(
        id=db_effect.id,
        name=db_effect.name,
        effect_type=db_effect.effect_type,
        active=db_effect.active,
        editable=db_effect.editable,
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

    if not db_effect.editable:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Notification effect is not editable",
        )

    db_effect.name = effect_data.name
    db_effect.effect_type = effect_data.effect_type
    db_effect.event_type = effect_data.event_type
    db_effect.frequency_days = effect_data.frequency_days
    db_effect.template = effect_data.template
    db_effect.subject = effect_data.subject
    db_effect.condition = effect_data.condition
    db_effect.conditional_parameters = effect_data.conditional_parameters

    session.commit()
    session.refresh(db_effect)

    # Return the updated effect
    return EffectOut(
        id=db_effect.id,
        name=db_effect.name,
        effect_type=db_effect.effect_type,
        active=db_effect.active,
        editable=db_effect.editable,
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
