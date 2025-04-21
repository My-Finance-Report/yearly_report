from decimal import Decimal
import re
from typing import List, Callable, Any
import enum 
from pydantic import BaseModel
from app.models import User
from app.email.send import Email, send_email
from app.schemas.no_code import NoCodeTransaction

class EffectType(str,enum.Enum):
    EMAIL = "email"
    IN_APP = "in_app"

class EffectConditionals(str,enum.Enum):
    AMOUNT_OVER = "amount_over"
    COUNT_OF_TRANSACTIONS = "count_of_transactions"

class NewTransactionsEvent(BaseModel):
    transactions: list[NoCodeTransaction]
    account_name: str

class NewAccountLinkedEvent(BaseModel):
    account_name: str


Event = NewTransactionsEvent | NewAccountLinkedEvent

def amount_over(event: NewTransactionsEvent, **kwargs) -> bool:
    return event.transactions[-1].amount > kwargs["amount"]

def count_of_transactions(event: NewTransactionsEvent, **kwargs) -> bool:
    return len(event.transactions) > kwargs["count"]

EFFECT_CONDITIONALS_LOOKUP: dict[EffectConditionals, Callable[[Event, Any], bool]] = {
    EffectConditionals.AMOUNT_OVER: amount_over,
    EffectConditionals.COUNT_OF_TRANSACTIONS: count_of_transactions
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

class EventType(str,enum.Enum):
    NEW_TRANSACTION = "new_transaction"
 


def collect_user_effects(user: User) -> List[Effect]:
    """
    Retrieve all notification effects configured by the user.
    #TODO pull these from the database for the user
    """

    return [
        Effect(
            type=EffectType.EMAIL,
            config=EffectConfig(
                frequency_days=1,
                template="Hey there! You have {count} new transaction(s) in My Financé!",
                subject="New Transactions in My Financé from {account_name}"
            ),
            condition=EffectConditionals.COUNT_OF_TRANSACTIONS,
            conditional_parameters={"count": 0}
        ),
    ]

def check_event_against_possible_effects(event: Event, effect: Effect) -> bool:
    """
    Check if the effect should be triggered for this event.
    This uses the user-configured condition (could be a lambda, or a rule engine).
    """
    try:
        callable = EFFECT_CONDITIONALS_LOOKUP[effect.condition]
        return callable(event, **effect.conditional_parameters)
    except Exception as e:
        print(f"Error evaluating condition: {e}")
        return False

def aggregate_effects(effects: List[Effect]) -> List[Effect]:
    """
    Group or deduplicate effects if needed.
    For example, combine multiple similar email notifications into one.
    """
    # TODO apply this
    aggregated = []
    seen = set()
    for effect in effects:
        key = (effect.type, effect.config.template)
        if key not in seen:
            aggregated.append(effect)
            seen.add(key)
    return aggregated


def perform_template_replacement(event: Event, effect: Effect) -> Email:
    template = effect.config.template

    # find vars in double brackets {{account_name}}
    vars = re.findall(r'{{\s*(\w+)\s*}}', template)
    for var in vars:
        if hasattr(event, var):
            value = str(getattr(event, var))
            # Replace all occurrences of {{var}} (with or without spaces)
            template = re.sub(r'{{\s*' + re.escape(var) + r'\s*}}', value, template)
    return Email(
        subject=effect.config.subject,
        html=template
    )


def generate_callable_for_effect(event: Event, effect: Effect) -> Callable[[], Email]:
    return lambda: Email(
        subject=effect.config.subject,
        html=perform_template_replacement(event, effect).html
    )

def propagate_effects(user:User,effects: List[Effect], event: Event):
    """
    Actually perform the effects (send emails, push notifications, etc).
    This is where you would call your email/SMS/push services.
    """
    for effect in effects:
        if effect.type == EffectType.EMAIL:
            print("calling send email")
            send_email(user, generate_callable_for_effect(event, effect))
        elif effect.type == EffectType.IN_APP:
            print("need to impliment")
            #send_in_app_notification(effect.config)

def trigger_effects(user: User, event: Event):
    """
    Main entry point: given a user and an event, trigger all matching effects.
    """
    effects = collect_user_effects(user)
    effects_to_trigger = [effect for effect in effects if check_event_against_possible_effects(event, effect)]
    propagate_effects(user, effects_to_trigger, event)


