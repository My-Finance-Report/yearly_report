from typing import Callable
from app.func_utils import not_none

from pydantic import BaseModel
from app.models.effect import ConditionalParameters, EffectConditionals, EffectType

from app.no_code.notifications.events import (
    BudgetThresholdExceededEvent,
    Event,
    NewTransactionsEvent,
)
from app.schemas.no_code import NoCodeBudgetEntry


def amount_over(event: Event, conditions: ConditionalParameters) -> bool:
    amount = not_none(conditions.amount)

    match event:
        case NewTransactionsEvent():
            return event.transactions[-1].amount > amount
        case BudgetThresholdExceededEvent():

            def amount_over(entry: NoCodeBudgetEntry) -> bool:
                current_percent = entry.current / entry.target
                return current_percent > amount

            return any(amount_over(entry) for entry in event.budget_entries)
        case _:
            return False


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
