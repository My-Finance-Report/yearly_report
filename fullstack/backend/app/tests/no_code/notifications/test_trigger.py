import pytest
from datetime import datetime, timedelta
from app.models.effect import EffectLog
from unittest.mock import patch, MagicMock, call
from app.no_code.notifications.effects import Effect, EffectConfig
from app.no_code.notifications.events import NewTransactionsEvent

from app.models.effect import EffectConditionals, EffectType
from app.no_code.notifications.trigger import (
    check_effects_against_frequency,
    check_event_against_possible_effects,
    aggregate_effects,
    perform_template_replacement,
    generate_callable_for_effect,
    propagate_effects,
    trigger_effects,
)
from app.models.transaction import TransactionKind
from app.schemas.no_code import NoCodeTransaction
from app.tests.utils.utils import TestKit


class DummySession:
    def __init__(self, logs):
        self._logs = logs
        self.added_items = []

    def query(self, model):
        class Query:
            def __init__(self, logs):
                self._logs = logs

            def filter(self, *args, **kwargs):
                return self

            def all(self):
                return self._logs

        return Query(self._logs)

    def add(self, item):
        self.added_items.append(item)
        return self

    def commit(self):
        # Just a dummy method that does nothing
        pass


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
            date_of_transaction=datetime(year=2024, month=1, day=1),
            description="Test transaction 1",
            kind=TransactionKind.withdrawal,
            category_name="Test Category",
            account_name="Test Account",
        ),
        NoCodeTransaction(
            amount=200,
            date_of_transaction=datetime(year=2024, month=1, day=2),
            description="Test transaction 2",
            kind=TransactionKind.withdrawal,
            category_name="Test Category",
            account_name="Test Account",
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
    return NewTransactionsEvent(
        transactions=transactions, account_name="TestAccount", count=2
    )


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


def test_substitutions():
    email = perform_template_replacement(
        NewTransactionsEvent(
            transactions=[],
            account_name="TestAccount",
            count=1,
        ),
        Effect(
            type=EffectType.EMAIL,
            config=EffectConfig(
                frequency_days=1,
                template="Hey there! You have {{ count }} new transaction(s) in My Financé!",
                subject="New Transactions in My Financé from {{ account_name }}",
            ),
            condition=EffectConditionals.COUNT_OF_TRANSACTIONS,
            conditional_parameters={"count": 0},
        ),
    )

    assert "New Transactions in My Financé from TestAccount" in email.subject
    assert "Hey there! You have 1 new transaction(s) in My Financé!" in email.html


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


def test_functional_pipeline(user, event):
    """Test the functional pipeline implementation in trigger_effects."""
    # Create a test effect
    effect = Effect(
        type=EffectType.EMAIL,
        config=EffectConfig(
            frequency_days=1, template="Account: {{account_name}}", subject="Subject"
        ),
        condition=EffectConditionals.COUNT_OF_TRANSACTIONS,
        conditional_parameters={"count": 0},
    )

    # Create a mock manager to track call order
    mock_manager = MagicMock()

    # Create mock functions with side effects to track the pipeline flow
    def collect_side_effect(session, user):
        mock_manager.collect_called()
        return [effect]

    def filter_side_effect(event, effects):
        mock_manager.filter_called()
        assert effects == [effect], "Filter received wrong effects"
        return [effect]

    def aggregate_side_effect(effects):
        mock_manager.aggregate_called()
        assert effects == [effect], "Aggregate received wrong effects"
        return [effect]

    def frequency_side_effect(session, user, effects):
        mock_manager.frequency_called()
        assert effects == [effect], "Frequency check received wrong effects"
        return [effect]

    def log_side_effect(session, user_id, event_obj, effects):
        mock_manager.log_called()
        assert effects == [effect], "Log received wrong effects"
        assert user_id == user.id, "Log received wrong user ID"
        assert event_obj == event, "Log received wrong event"
        return [effect]

    def propagate_side_effect(user_obj, effects, event_obj):
        mock_manager.propagate_called()
        assert effects == [effect], "Propagate received wrong effects"
        assert user_obj == user, "Propagate received wrong user"
        assert event_obj == event, "Propagate received wrong event"

    # Patch all the pipeline functions with side effects
    with (
        patch(
            "app.no_code.notifications.trigger.collect_user_effects",
            side_effect=collect_side_effect,
        ),
        patch(
            "app.no_code.notifications.trigger.check_all_effects",
            side_effect=filter_side_effect,
        ),
        patch(
            "app.no_code.notifications.trigger.aggregate_effects",
            side_effect=aggregate_side_effect,
        ),
        patch(
            "app.no_code.notifications.trigger.check_effects_against_frequency",
            side_effect=frequency_side_effect,
        ),
        patch(
            "app.no_code.notifications.trigger.record_effects_log",
            side_effect=log_side_effect,
        ),
        patch(
            "app.no_code.notifications.trigger.propagate_effects",
            side_effect=propagate_side_effect,
        ),
    ):
        session = MagicMock()

        # Call the function under test
        trigger_effects(session, user, event)

        # Verify that each function was called exactly once
        assert mock_manager.collect_called.call_count == 1, (
            "collect_user_effects not called exactly once"
        )
        assert mock_manager.filter_called.call_count == 1, (
            "check_all_effects not called exactly once"
        )
        assert mock_manager.aggregate_called.call_count == 1, (
            "aggregate_effects not called exactly once"
        )
        assert mock_manager.frequency_called.call_count == 1, (
            "check_effects_against_frequency not called exactly once"
        )
        assert mock_manager.log_called.call_count == 1, (
            "record_effects_log not called exactly once"
        )
        assert mock_manager.propagate_called.call_count == 1, (
            "propagate_effects not called exactly once"
        )

        # Get the actual call order from the mock manager's mock_calls
        actual_calls = [str(call) for call in mock_manager.mock_calls[:6]]

        # Define the expected call order
        expected_calls = [
            "call.collect_called()",
            "call.filter_called()",
            "call.aggregate_called()",
            "call.frequency_called()",
            "call.log_called()",
            "call.propagate_called()",
        ]

        # Verify the call order is correct
        assert actual_calls == expected_calls, (
            f"Incorrect call order. Expected: {expected_calls}, Actual: {actual_calls}"
        )
