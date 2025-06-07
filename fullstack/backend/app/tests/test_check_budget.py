import pytest
from datetime import datetime
from decimal import Decimal
from sqlalchemy.orm import Session

from app.local_types import (
    BudgetCategoryLinkOut,
    BudgetCategoryLinkStatus,
    BudgetEntryOut,
    BudgetEntryStatus,
    BudgetOut,
    BudgetStatus,
    Month,
)
from app.models.user import User
from app.models.budget import Budget, BudgetEntry, BudgetCategoryLink
from app.models.category import Category
from app.models.transaction import Transaction, TransactionKind
from app.models.transaction_source import TransactionSource
from app.budgets.check_budget import (
    get_stylized_name_lookup,
    group_transactions_by_month,
    create_budget,
    build_budget_out,
    build_budget_status,
)


@pytest.fixture
def test_user(pglite_session: Session):
    user = User(name="Test User", email="test@example.com")
    pglite_session.add(user)
    pglite_session.commit()
    pglite_session.refresh(user)
    return user


@pytest.fixture
def test_source(pglite_session: Session, test_user: User):
    source = TransactionSource(name="Test Source", user_id=test_user.id)
    pglite_session.add(source)
    pglite_session.commit()
    pglite_session.refresh(source)
    return source


@pytest.fixture
def test_category(
    pglite_session: Session, test_user: User, test_source: TransactionSource
):
    category = Category(
        name="Test Category", user_id=test_user.id, source_id=test_source.id
    )
    pglite_session.add(category)
    pglite_session.commit()
    pglite_session.refresh(category)
    return category


@pytest.fixture
def test_budget(pglite_session: Session, test_user: User):
    budget = Budget(name="Test Budget", user_id=test_user.id, active=True)
    pglite_session.add(budget)
    pglite_session.commit()
    pglite_session.refresh(budget)
    return budget


@pytest.fixture
def test_budget_entry(pglite_session: Session, test_budget: Budget, test_user: User):
    entry = BudgetEntry(
        name="Test Entry",
        budget_id=test_budget.id,
        amount=Decimal("1000.00"),
        user_id=test_user.id,
    )
    pglite_session.add(entry)
    pglite_session.commit()
    pglite_session.refresh(entry)
    return entry


@pytest.fixture
def test_budget_category_link(
    pglite_session: Session,
    test_budget_entry: BudgetEntry,
    test_category: Category,
    test_user: User,
):
    link = BudgetCategoryLink(
        budget_entry_id=test_budget_entry.id,
        category_id=test_category.id,
        user_id=test_user.id,
    )
    pglite_session.add(link)
    pglite_session.commit()
    pglite_session.refresh(link)
    return link


def test_get_stylized_name_lookup(
    pglite_session: Session,
    test_user: User,
    test_category: Category,
    test_source: TransactionSource,
):
    lookup = get_stylized_name_lookup(pglite_session, test_user)
    assert lookup[test_category.id] == f"{test_category.name} ({test_source.name})"


def test_group_transactions_by_month(
    pglite_session: Session, test_user: User, test_category: Category
):
    # Create test transactions in different months
    transactions = [
        Transaction(
            user_id=test_user.id,
            category_id=test_category.id,
            amount=Decimal("100.00"),
            date_of_transaction=datetime(2025, 1, 15),
            kind=TransactionKind.withdrawal,
            name="Jan Transaction",
        ),
        Transaction(
            user_id=test_user.id,
            category_id=test_category.id,
            amount=Decimal("200.00"),
            date_of_transaction=datetime(2025, 2, 15),
            kind=TransactionKind.withdrawal,
            name="Feb Transaction",
        ),
    ]
    pglite_session.add_all(transactions)
    pglite_session.commit()

    grouped = group_transactions_by_month(transactions)
    jan = Month("2025-01")
    feb = Month("2025-02")
    assert jan in grouped
    assert feb in grouped
    assert len(grouped[jan]) == 1
    assert len(grouped[feb]) == 1
    assert grouped[jan][0].amount == Decimal("100.00")
    assert grouped[feb][0].amount == Decimal("200.00")


def test_create_budget(pglite_session: Session, test_user: User):
    budget = create_budget(pglite_session, test_user)
    assert budget.name == "Budget"
    assert budget.user_id == test_user.id
    assert budget.active is True


def test_build_budget_out(
    pglite_session: Session,
    test_user: User,
    test_budget: Budget,
    test_budget_entry: BudgetEntry,
    test_budget_category_link: BudgetCategoryLink,
):
    budget_out = build_budget_out(pglite_session, test_user)
    assert budget_out.id == test_budget.id
    assert budget_out.name == test_budget.name
    assert budget_out.active is True
    assert len(budget_out.entries) == 1

    entry = budget_out.entries[0]
    assert entry.id == test_budget_entry.id
    assert entry.name == test_budget_entry.name
    assert entry.amount == test_budget_entry.amount
    assert len(entry.category_links) == 1

    link = entry.category_links[0]
    assert link.id == test_budget_category_link.id
    assert link.category_id == test_budget_category_link.category_id


def test_build_budget_status(
    pglite_session: Session,
    test_user: User,
    test_budget: Budget,
    test_budget_entry: BudgetEntry,
    test_budget_category_link: BudgetCategoryLink,
    test_category: Category,
):
    # Create some test transactions
    transactions = [
        Transaction(
            user_id=test_user.id,
            category_id=test_category.id,
            amount=Decimal("100.00"),
            date_of_transaction=datetime(2025, 1, 15),
            kind=TransactionKind.withdrawal,
            name="Jan Transaction",
        ),
        Transaction(
            user_id=test_user.id,
            category_id=test_category.id,
            amount=Decimal("200.00"),
            date_of_transaction=datetime(2025, 2, 15),
            kind=TransactionKind.withdrawal,
            name="Feb Transaction",
        ),
    ]
    pglite_session.add_all(transactions)
    pglite_session.commit()

    budget_status = build_budget_status(pglite_session, test_user)
    assert budget_status.budget_id == test_budget.id
    assert budget_status.name == test_budget.name
    assert budget_status.active is True
    assert len(budget_status.entry_status) == 1

    entry_status = budget_status.entry_status[0]
    assert entry_status.id == test_budget_entry.id
    assert entry_status.name == test_budget_entry.name
    assert entry_status.amount == test_budget_entry.amount
    assert entry_status.yearly_total == Decimal("300.00")  # Sum of all transactions

    jan = Month("2025-01")
    feb = Month("2025-02")
    assert jan in budget_status.months_with_entries
    assert feb in budget_status.months_with_entries

    # Check category status for each month
    jan_status = entry_status.category_links_status[jan]
    assert jan_status.monthly_total == Decimal("100.00")
    assert len(jan_status.transactions) == 1

    feb_status = entry_status.category_links_status[feb]
    assert feb_status.monthly_total == Decimal("200.00")
    assert len(feb_status.transactions) == 1
