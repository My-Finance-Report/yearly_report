import re
from datetime import datetime, timedelta
from typing import List, Callable
from app.no_code.notifications.effects import (
    EFFECT_CONDITIONALS_LOOKUP,
    Effect,
    EffectConfig,
)
from app.no_code.notifications.events import Event
from sqlalchemy.orm import Session
from app.models.effect import EffectConditionals, EffectLog, EffectType
from app.models.user import User
from app.email.send import Email, send_email


def collect_user_effects(session: Session, user: User) -> List[Effect]:
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
                subject="New Transactions in My Financé from {account_name}",
            ),
            condition=EffectConditionals.COUNT_OF_TRANSACTIONS,
            conditional_parameters={"count": 0},
        ),
    ]


def check_event_against_possible_effects(event: Event, effect: Effect) -> bool:
    """
    Check if the effect should be triggered for this event.
    This uses the user-configured condition (could be a lambda, or a rule engine).
    """
    try:
        callable = EFFECT_CONDITIONALS_LOOKUP[effect.condition]
        return callable(event, **effect.conditional_parameters)  # type: ignore[call-arg]
    except Exception as e:
        print(f"Error evaluating condition: {e}")
        return False


def check_effects_against_frequency(
    session: Session, user: User, effects: List[Effect]
) -> List[Effect]:
    if not effects:
        return []

    max_days = max(e.config.frequency_days for e in effects)
    window_start = datetime.now() - timedelta(days=max_days)

    effect_logs = (
        session.query(EffectLog)
        .filter(EffectLog.user_id == user.id)
        .filter(EffectLog.fired_at >= window_start)
        .all()
    )

    to_fire_effects = []
    for effect in effects:
        frequency = effect.config.frequency_days
        # Find last fired log for this effect type
        last_fired = max(
            (log.fired_at for log in effect_logs if log.effect_type == effect.type),
            default=None,
        )
        if not last_fired or last_fired < datetime.now() - timedelta(days=frequency):
            to_fire_effects.append(effect)

    return to_fire_effects


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
    vars = re.findall(r"{{\s*(\w+)\s*}}", template)
    for var in vars:
        if hasattr(event, var):
            value = str(getattr(event, var))
            # Replace all occurrences of {{var}} (with or without spaces)
            template = re.sub(r"{{\s*" + re.escape(var) + r"\s*}}", value, template)
    return Email(subject=effect.config.subject, html=template)


def generate_callable_for_effect(event: Event, effect: Effect) -> Callable[[], Email]:
    return lambda: Email(
        subject=effect.config.subject,
        html=perform_template_replacement(event, effect).html,
    )


def propagate_effects(user: User, effects: List[Effect], event: Event) -> None:
    """
    Actually perform the effects (send emails, push notifications, etc).
    This is where you would call your email/SMS/push services.
    """
    for effect in effects:
        if effect.type == EffectType.EMAIL:
            send_email(user, generate_callable_for_effect(event, effect))
        else:
            raise NotImplementedError()


def trigger_effects(session: Session, user: User, event: Event) -> None:
    """
    Main entry point: given a user and an event, trigger all matching effects.
    """
    effects = collect_user_effects(session, user)
    effects_to_trigger = [
        effect
        for effect in effects
        if check_event_against_possible_effects(event, effect)
    ]
    effects_to_trigger = check_effects_against_frequency(
        session, user, effects_to_trigger
    )
    propagate_effects(user, effects_to_trigger, event)
