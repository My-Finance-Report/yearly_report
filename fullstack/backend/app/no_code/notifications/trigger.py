import re
from datetime import datetime, timedelta, timezone
from typing import List, Callable
from app.no_code.notifications.effect_generators.seed_effects import (
    new_transaction_effect,
)
from app.no_code.notifications.effects import (
    EFFECT_CONDITIONALS_LOOKUP,
    Effect,
)

from app.no_code.notifications.events import Event
from sqlalchemy.orm import Session
from app.models.effect import EffectLog, EffectType
from app.models.user import User, UserId
from app.email.send import Email, send_email


def collect_user_effects(session: Session, user: User) -> List[Effect]:
    """
    Retrieve all notification effects configured by the user.
    """

    return [
        new_transaction_effect(session, user),
    ]


def check_all_effects(event: Event, effects: List[Effect]) -> List[Effect]:
    return [
        effect
        for effect in effects
        if check_event_against_possible_effects(event, effect)
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
    window_start = datetime.now(timezone.utc) - timedelta(days=max_days)

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
    aggregated = []
    seen = set()
    for effect in effects:
        key = (effect.type, effect.config.template)
        if key not in seen:
            aggregated.append(effect)
            seen.add(key)
    return aggregated


def perform_template_replacement(event: Event, effect: Effect) -> Email:
    # find vars in double brackets {{account_name}}

    def make_subs(temp: str) -> str:
        vars = re.findall(r"{{\s*(\w+)\s*}}", temp)
        for var in vars:
            if hasattr(event, var):
                value = str(getattr(event, var))
                # Replace all occurrences of {{var}} (with or without spaces)
                temp = re.sub(r"{{\s*" + re.escape(var) + r"\s*}}", value, temp)
        return temp

    subject = make_subs(effect.config.subject)
    body = make_subs(effect.config.template)
    return Email(subject=subject, html=body)


def generate_callable_for_effect(event: Event, effect: Effect) -> Callable[[], Email]:
    return lambda: perform_template_replacement(event, effect)


def record_effects_log(
    session: Session, user_id: UserId, event: Event, effects: list[Effect]
) -> list[Effect]:
    for effect in effects:
        log = EffectLog(
            effect_type=effect.type,
            event_type=event.type,
            user_id=user_id,
        )
        session.add(log)
    session.commit()
    return effects


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

    def collect(_: list[Effect]) -> list[Effect]:
        return collect_user_effects(session, user)

    def filter_effects(effects: list[Effect]) -> list[Effect]:
        return check_all_effects(event, effects)

    def should_fire_check(effects: list[Effect]) -> list[Effect]:
        return check_effects_against_frequency(session, user, effects)

    def log(effects: list[Effect]) -> list[Effect]:
        return record_effects_log(session, user.id, event, effects)

    def propagate(effects: list[Effect]) -> list[Effect]:
        propagate_effects(user, effects, event)
        return effects

    pipeline: list[Callable[[list[Effect]], list[Effect]]] = [
        collect,
        filter_effects,
        aggregate_effects,
        should_fire_check,
        log,
        propagate,
    ]

    arg: list[Effect] = []
    for callable in pipeline:
        print(f"Applying {callable.__name__} to {arg}")
        arg = callable(arg)
