from collections import defaultdict
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
    TransactionOut,
)
from app.models.budget import Budget, BudgetCategoryLink, BudgetEntry, BudgetEntryId
from app.models.transaction import Transaction, TransactionKind
from app.models.user import User
from app.models.category import Category, CategoryId
from app.models.transaction_source import TransactionSource

def get_stylized_name_lookup(session: Session, user: User) -> dict[CategoryId, str]:
    categories = (
        session.query(Category, TransactionSource)
        .join(TransactionSource, TransactionSource.id == Category.source_id)
        .filter(Category.user_id == user.id)
        .all()
    )
    return {
        category.id: f"{category.name} ({source.name})"
        for category, source in categories
    }



def group_transactions_by_month(
    transactions: list[Transaction],
) -> dict[str, list[Transaction]]:
    grouped_transactions = defaultdict(list)

    for transaction in transactions:
        month = transaction.date_of_transaction.strftime("%Y-%m")
        grouped_transactions[month].append(transaction)

    return dict(grouped_transactions)




def create_budget(session: Session, user: User) -> Budget:
    new_budget = Budget(name="Budget", user_id=user.id, active=True)
    session.add(new_budget)
    session.commit()
    return new_budget



def build_budget_out(session: Session, user: User) -> BudgetOut:
    budget = session.query(Budget).filter(Budget.user_id == user.id).first()

    if not budget:
        budget = create_budget(session=session, user=user)

    entries = (
        session.query(BudgetEntry)
        .filter(BudgetEntry.budget_id == budget.id, BudgetEntry.user_id == user.id)
        .all()
    )

    entry_ids = [entry.id for entry in entries]
    categories = (
        session.query(BudgetCategoryLink)
        .filter(
            BudgetCategoryLink.budget_entry_id.in_(entry_ids),
            BudgetCategoryLink.user_id == user.id,
        )
        .all()
    )

    category_by_entry: dict[BudgetEntryId, list[BudgetCategoryLinkOut]] = defaultdict(
        list
    )

    name_lookup = get_stylized_name_lookup(session, user)

    for category in categories:
        category_by_entry[category.budget_entry_id].append(
            BudgetCategoryLinkOut(
                id=category.id,
                budget_entry_id=category.budget_entry_id,
                category_id=category.category_id,
                stylized_name=name_lookup[category.category_id],
            )
        )

    entries_out = [
        BudgetEntryOut(
            id=entry.id,
            name=entry.name,
            budget_id=entry.budget_id,
            amount=entry.amount,
            user_id=entry.user_id,
            category_links=category_by_entry[entry.id],
        )
        for entry in entries
    ]
    return BudgetOut(
        active=budget.active,
        id=budget.id,
        user_id=budget.user_id,
        name=budget.name,
        entries=entries_out,
    )




def build_budget_status(
    session: Session,
    user: User,
) -> BudgetStatus:
    budget = build_budget_out(session=session, user=user)

    entry_statuses = []
    months_with_entries = set()
    for entry in budget.entries:
        category_status: dict[Month, BudgetCategoryLinkStatus] = {}
        running_total: Decimal = Decimal(0)
        for category in entry.category_links:
            transactions = (
                session.query(Transaction)
                .filter(Transaction.category_id == category.category_id)
                .all()
            )

            for month, month_transactions in group_transactions_by_month(
                transactions
            ).items():
                transactions_out = [
                    TransactionOut(**transaction.__dict__)
                    for transaction in month_transactions
                    if transaction.kind == TransactionKind.withdrawal
                ]
                total = Decimal(sum([t.amount for t in transactions_out]))
                running_total += total
                months_with_entries.add(Month(month))
                category_status[Month(month)] = BudgetCategoryLinkStatus(
                    budget_entry_id=category.budget_entry_id,
                    id=category.id,
                    stylized_name=category.stylized_name,
                    category_id=category.category_id,
                    transactions=transactions_out,
                    total=total,
                )

        entry_statuses.append(
            BudgetEntryStatus(
                id=entry.id,
                budget_id=budget.id,
                name=entry.name,
                amount=entry.amount,
                category_links_status=category_status,
                target=entry.amount,
                total=running_total,
            )
        )

    return BudgetStatus(
        budget_id=budget.id,
        user_id=user.id,
        name=budget.name,
        active=True,
        entries=budget.entries,
        entry_status=entry_statuses,
        months_with_entries=list(months_with_entries),
    )


