from fastapi import APIRouter, Depends, HTTPException
from typing import Optional
from datetime import datetime, timedelta

from app.db import Session, get_current_user, get_db
from app.email.send import Email
from app.local_types import EffectOut
from app.models.effect import EffectConditionals, EffectType, EventType
from app.models.user import User
from app.models.transaction import TransactionKind
from app.no_code.notifications.effect_generators.seed_effects import new_transaction_effect
from app.no_code.notifications.effects import EffectConfig, Effect
from app.no_code.notifications.events import NewTransactionsEvent
from app.no_code.notifications.trigger import perform_template_replacement
from app.schemas.no_code import NoCodeTransaction

router = APIRouter(prefix="/notification", tags=["no_code"])


@router.get("/get_effects", response_model=list[EffectOut])
def get_effects(
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> list[EffectOut]:

    transaction_effect = new_transaction_effect(session, user)
    return [
        EffectOut(
            effect_type=transaction_effect.type,
            name="New Transactions",
            event_type=EventType.NEW_TRANSACTION,
            config=transaction_effect.config,
            condition=transaction_effect.condition,
            conditional_parameters=transaction_effect.conditional_parameters,
        ),
        EffectOut(
            effect_type=EffectType.EMAIL,
            name="Big Spender",
            event_type=EventType.NEW_TRANSACTION,
            config=EffectConfig(
                frequency_days=1,
                template="Hey there big spender! You have a transaction over {{amount}} in My Financé!",
                subject="New Transactions in My Financé from {{account_name}}",
            ),
            condition=EffectConditionals.AMOUNT_OVER,
            conditional_parameters={"amount": 1000, "comparator": ">"},
        ),
    ]


class NotificationPreviewResponse:
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
                amount=amount,
                date_of_transaction=date,
                description=f"Sample Transaction {i+1}",
                kind=kind,
                category_name=f"Category {(i % 3) + 1}",
                account_name=account_name,
            )
        )
    
    # Create the event
    event = NewTransactionsEvent(
        transactions=sample_transactions,
        account_name=account_name,
        count=len(sample_transactions)
    )
    
    # Use provided template and subject or defaults
    config = EffectConfig(
        frequency_days=1,
        template=template or "Hey there! You have {{ count }} new transaction(s) in My Financé!\n{{ transactions_table }}\n{{ alter_settings }}",
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
    
