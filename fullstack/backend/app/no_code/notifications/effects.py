from typing import Callable
from app.func_utils import not_none

from pydantic import BaseModel
from app.models.effect import ConditionalParameters, EffectConditionals, EffectType

from app.no_code.notifications.events import (
    AccountDeactivatedEvent,
    Event,
    NewAccountLinkedEvent,
    NewTransactionsEvent,
)


def amount_over(event: Event, conditions: ConditionalParameters) -> bool:
    assert not_none(conditions.amount)
    assert isinstance(event, NewTransactionsEvent)

    return event.transactions[-1].amount > conditions.amount


def count_of_transactions(event: Event, conditions: ConditionalParameters) -> bool:
    assert not_none(conditions.count)
    assert isinstance(event, NewTransactionsEvent)
    return len(event.transactions) > conditions.count


def unconditional(_event: Event, _conditions: ConditionalParameters) -> bool:
    return True


EFFECT_CONDITIONALS_LOOKUP: dict[
    EffectConditionals, Callable[[Event, ConditionalParameters], bool]
] = {
    EffectConditionals.AMOUNT_OVER: amount_over,
    EffectConditionals.COUNT_OF_TRANSACTIONS: count_of_transactions,
    EffectConditionals.UNCONDITIONAL: unconditional,
}


class EffectConfig(BaseModel):
    frequency_days: int
    template: str
    subject: str


class Effect(BaseModel):
    active: bool
    editable: bool
    type: EffectType
    config: EffectConfig
    condition: EffectConditionals
    conditional_parameters: ConditionalParameters
