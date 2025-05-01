from typing import Callable, Any, cast

from pydantic import BaseModel
from app.models.effect import EffectConditionals, EffectType

from app.no_code.notifications.events import NewTransactionsEvent


def amount_over(event: NewTransactionsEvent, **kwargs: dict[str, Any]) -> bool:
    return event.transactions[-1].amount > cast(float, kwargs["amount"])


def count_of_transactions(
    event: NewTransactionsEvent, **kwargs: dict[str, Any]
) -> bool:
    return len(event.transactions) > cast(int, kwargs["count"])


EFFECT_CONDITIONALS_LOOKUP: dict[EffectConditionals, Callable[[Any, Any], bool]] = {
    EffectConditionals.AMOUNT_OVER: amount_over,  # type: ignore[dict-item]
    EffectConditionals.COUNT_OF_TRANSACTIONS: count_of_transactions,  # type: ignore[dict-item]
}


class EffectConfig(BaseModel):
    frequency_days: int
    template: str
    subject: str


class Effect(BaseModel):
    type: EffectType
    config: EffectConfig
    condition: EffectConditionals
    conditional_parameters: dict[str, Any]
