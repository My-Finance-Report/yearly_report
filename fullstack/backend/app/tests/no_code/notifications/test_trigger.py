import pytest
from datetime import datetime, timedelta
from app.models import EffectLog
from unittest.mock import patch, MagicMock
from app.no_code.notifications.effects import Effect, EffectConfig
from app.no_code.notifications.events import NewTransactionsEvent
from app.no_code.notifications.trigger import (
    EffectType,
    EffectConditionals,
    check_effects_against_frequency,
    check_event_against_possible_effects,
    aggregate_effects,
    perform_template_replacement,
    generate_callable_for_effect,
    propagate_effects,
    trigger_effects,
)
from app.models import TransactionKind
from app.schemas.no_code import NoCodeTransaction


class DummySession:
    def __init__(self, logs):
        self._logs = logs

    def query(self, model):
        class Query:
            def __init__(self, logs):
                self._logs = logs

            def filter(self, *args, **kwargs):
                return self

            def all(self):
                return self._logs

        return Query(self._logs)


class DummyUser:
    def __init__(self, email="test@example.com"):
        self.email = email
        self.send_email = True
        self.id = 1


@pytest.fixture
def user():
    return DummyUser()


@pytest.fixture
def transactions():
    return [
        NoCodeTransaction(
            amount=100,
            id="1",
            date="2024-01-01",
            description="Test transaction 1",
            date_of_transaction="2024-01-01",
            kind=TransactionKind.withdrawal,
            category_name="Test Category",
        ),
        NoCodeTransaction(
            amount=200,
            id="2",
            date="2024-01-02",
            description="Test transaction 2",
            date_of_transaction="2024-01-02",
            kind=TransactionKind.withdrawal,
            category_name="Test Category",
        ),
    ]


@pytest.fixture
def effect():
    return Effect(
        type=EffectType.EMAIL,
        config=EffectConfig(frequency_days=1, template="...", subject="..."),
        condition=EffectConditionals.COUNT_OF_TRANSACTIONS,
        conditional_parameters={"count": 0},
    )


@pytest.fixture
def event(transactions: list[NoCodeTransaction]):
    return NewTransactionsEvent(transactions=transactions, account_name="TestAccount")


def test_amount_over(event):
    effect = Effect(
        type=EffectType.EMAIL,
        config=EffectConfig(frequency_days=1, template="...", subject="..."),
        condition=EffectConditionals.AMOUNT_OVER,
        conditional_parameters={"amount": 150},
    )
    assert check_event_against_possible_effects(event, effect) == True


def test_count_of_transactions(event: NewTransactionsEvent):
    effect = Effect(
        type=EffectType.EMAIL,
        config=EffectConfig(frequency_days=1, template="...", subject="..."),
        condition=EffectConditionals.COUNT_OF_TRANSACTIONS,
        conditional_parameters={"count": 1},
    )
    assert check_event_against_possible_effects(event, effect) == True


def test_aggregate_effects(event):
    effect1 = Effect(
        type=EffectType.EMAIL,
        config=EffectConfig(frequency_days=1, template="foo", subject="..."),
        condition=EffectConditionals.COUNT_OF_TRANSACTIONS,
        conditional_parameters={"count": 0},
    )
    effect2 = Effect(
        type=EffectType.EMAIL,
        config=EffectConfig(frequency_days=1, template="foo", subject="..."),
        condition=EffectConditionals.AMOUNT_OVER,
        conditional_parameters={"amount": 0},
    )
    # Both have same template, should deduplicate
    agg = aggregate_effects([effect1, effect2])
    assert len(agg) == 1


def test_perform_template_replacement(event):
    effect = Effect(
        type=EffectType.EMAIL,
        config=EffectConfig(
            frequency_days=1, template="Account: {{account_name}}", subject="Subject"
        ),
        condition=EffectConditionals.COUNT_OF_TRANSACTIONS,
        conditional_parameters={"count": 0},
    )
    email = perform_template_replacement(event, effect)
    assert "TestAccount" in email.html


def test_generate_callable_for_effect(event):
    effect = Effect(
        type=EffectType.EMAIL,
        config=EffectConfig(
            frequency_days=1, template="Account: {{account_name}}", subject="Subject"
        ),
        condition=EffectConditionals.COUNT_OF_TRANSACTIONS,
        conditional_parameters={"count": 0},
    )
    email_callable = generate_callable_for_effect(event, effect)
    email = email_callable()
    assert "TestAccount" in email.html


def test_propagate_effects_calls_send_email(user, event):
    effect = Effect(
        type=EffectType.EMAIL,
        config=EffectConfig(
            frequency_days=1, template="Account: {{account_name}}", subject="Subject"
        ),
        condition=EffectConditionals.COUNT_OF_TRANSACTIONS,
        conditional_parameters={"count": 0},
    )
    with patch("app.no_code.notifications.trigger.send_email") as mock_send:
        propagate_effects(user, [effect], event)
        assert mock_send.called


def test_trigger_effects_integrated(user, event):
    # Patch collect_user_effects to return a known effect
    effect = Effect(
        type=EffectType.EMAIL,
        config=EffectConfig(
            frequency_days=1, template="Account: {{account_name}}", subject="Subject"
        ),
        condition=EffectConditionals.COUNT_OF_TRANSACTIONS,
        conditional_parameters={"count": 0},
    )
    with (
        patch(
            "app.no_code.notifications.trigger.collect_user_effects",
            return_value=[effect],
        ),
        patch("app.no_code.notifications.trigger.send_email") as mock_send,
    ):
        trigger_effects(DummySession([]), user, event)
        assert mock_send.called


def make_log(effect_type, days_ago):
    return EffectLog(
        id=1,
        effect_type=effect_type,
        event_type=None,
        user_id=1,
        fired_at=datetime.now() - timedelta(days=days_ago),
    )


def test_check_effects_against_frequency_no_logs(effect, user):
    session = DummySession([])
    result = check_effects_against_frequency(session, user, [effect])
    assert effect in result


def test_check_effects_against_frequency_recent_log(effect, user):
    # Log fired today, frequency is 1 day, should NOT fire
    log = make_log(effect.type, 0)
    session = DummySession([log])
    result = check_effects_against_frequency(session, user, [effect])
    assert effect not in result


def test_check_effects_against_frequency_old_log(effect, user):
    # Log fired 2 days ago, frequency is 1 day, should fire
    log = make_log(effect.type, 2)
    session = DummySession([log])
    result = check_effects_against_frequency(session, user, [effect])
    assert effect in result


def test_check_effects_against_frequency_empty_effects(user):
    session = DummySession([])
    result = check_effects_against_frequency(session, user, [])
    assert result == []


def test_trigger_effects_only_triggers_by_frequency(user, event, effect):
    # Patch frequency check to only return our effect if a flag is set
    with (
        patch(
            "app.no_code.notifications.trigger.check_effects_against_frequency",
            return_value=[effect],
        ),
        patch("app.no_code.notifications.trigger.send_email") as mock_send,
    ):
        session = MagicMock()
        trigger_effects(session, user, event)
        assert mock_send.called

    with (
        patch(
            "app.no_code.notifications.trigger.check_effects_against_frequency",
            return_value=[],
        ),
        patch("app.no_code.notifications.trigger.send_email") as mock_send,
    ):
        session = MagicMock()
        trigger_effects(session, user, event)
        assert not mock_send.called
