import re
from typing import cast
from html_sanitizer import Sanitizer
from datetime import datetime, timedelta, timezone
from typing import List, Callable
from app.no_code.notifications.effect_generators.seed_effects import (
    deactivated_account_effect,
    new_transaction_effect,
)
from app.no_code.notifications.effects import (
    EFFECT_CONDITIONALS_LOOKUP,
    Effect,
    EffectConfig,
)

from app.no_code.notifications.events import Event
from sqlalchemy.orm import Session
from app.models.effect import EffectLog, EffectType, Effect as EffectModel, EventType
from app.models.user import User, UserId
from app.email.send import Email, send_email


def collect_user_effects(
    session: Session, user: User, event_type: EventType
) -> List[Effect]:
    """
    Retrieve all notification effects configured by the user.
    """
    effects = session.query(EffectModel).filter(
        EffectModel.user_id == user.id,
        EffectModel.event_type == event_type,
    )

    db_effects = [
        Effect(
            type=effect.effect_type,
            condition=effect.condition,
            conditional_parameters=effect.conditional_parameters,
            config=EffectConfig(
                template=effect.template,
                subject=effect.subject,
                frequency_days=effect.frequency_days,
            ),
        )
        for effect in effects
    ]

    return [
        *db_effects,
        new_transaction_effect(session, user),
        deactivated_account_effect(session, user),
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
        return callable(event, effect.conditional_parameters)
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

    return Email(subject=subject, clean_html=purify_html(body))


def purify_html(html: str) -> str:
    sanitizer = Sanitizer(
        settings={
            "tags": {
                # Text formatting
                "p",
                "strong",
                "em",
                "b",
                "i",
                "u",
                # Headers
                "h1",
                "h2",
                "h3",
                "h4",
                "h5",
                "h6",
                # Lists
                "ul",
                "ol",
                "li",
                # Tables
                "table",
                "thead",
                "tbody",
                "tr",
                "th",
                "td",
                # Other
                "br",
                "hr",
                "a",
                "span",
                "div",
            },
            "attributes": {
                "a": ("href", "title", "target", "rel"),
                "table": ("class", "style"),
                "th": ("colspan", "rowspan", "style"),
                "td": ("colspan", "rowspan", "style"),
                "span": ("class", "style"),
                "div": ("class", "style"),
            },
            "empty": {"br", "hr", "td", "th"},
            "separate": {"table", "thead", "tbody", "tr", "th", "td", "div", "p", "li"},
            "add_nofollow": True,  # Security: add rel="nofollow" to links
            "autolink": False,  # Don't auto-convert URLs to links
        }
    )
    return cast(str, sanitizer.sanitize(html))


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
        return collect_user_effects(session, user, event.type)

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
        arg = callable(arg)
