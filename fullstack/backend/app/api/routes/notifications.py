
from fastapi import APIRouter, Depends

from app.db import Session, get_current_user, get_db
from app.local_types import EffectOut
from app.models import EffectType, User, EventType
from app.no_code.notifications.effects import EffectConditionals, EffectConfig

router = APIRouter(prefix="/notification", tags=["no_code"])

@router.get("/get_effects", response_model=list[EffectOut])
def get_effects(
    session: Session = Depends(get_db),
    user: User= Depends(get_current_user),
) -> list[EffectOut]:
    return [
        EffectOut(
            effect_type=EffectType.EMAIL,
            name="Notify on new transactions",
            event_type=EventType.NEW_TRANSACTION,
            config=EffectConfig(
                frequency_days=1,
                template="Hey there! You have {{count}} new transaction(s) in My Financé!",
                subject="{{count}} new transactions have been uploaded from {{account_name}}.",
            ),
            condition=EffectConditionals.COUNT_OF_TRANSACTIONS,
            conditional_parameters={"count": 0, "comparator": ">"},
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


