from fastapi import APIRouter

from app.api.routes.demo_data import get_demo_data
from app.api.routes.transactions import BudgetLookup, recursive_grouping
from app.local_types import (
    AggregatedTransactions,
)
from app.models import (
    Category,
    CategoryId,
    FilterData,
    FilterEntries,
    GroupByOption,
    Transaction,
    TransactionSource,
    TransactionSourceId,
)

router = APIRouter(prefix="/demo", tags=["demo"])


def apply_category_filter(
    transactions: list[Transaction],
    categories: list[CategoryId],
) -> list[Transaction]:
    return [t for t in transactions if t.category_id in categories]


def apply_account_filter(
    transactions: list[Transaction],
    accounts: list[TransactionSourceId],
) -> list[Transaction]:
    return [t for t in transactions if t.transaction_source_id in accounts]


def apply_year_filter(
    transactions: list[Transaction],
    years: FilterEntries,
) -> list[Transaction]:
    if years.all:
        return transactions
    return [
        t
        for t in transactions
        if t.date_of_transaction.year in [int(y.value) for y in years.specifics or []]
    ]


def apply_month_filter(
    transactions: list[Transaction],
    months: FilterEntries,
) -> list[Transaction]:
    if months.all:
        return transactions
    month_numbers = (
        [int(m.value.lower()) for m in months.specifics or []]
        if months.specifics
        else []
    )

    print(month_numbers)
    print([t.date_of_transaction.month for t in transactions])
    return [t for t in transactions if t.date_of_transaction.month in month_numbers]


def get_demo_grouping_options(
    transactions: list[Transaction],
    category_lookup: dict[CategoryId, Category],
    ts_lookup: dict[TransactionSourceId, TransactionSource],
) -> dict[GroupByOption, list[str]]:
    val = {
        GroupByOption.category: [
            category_lookup[t.category_id].name for t in transactions
        ],
        GroupByOption.month: [
            t.date_of_transaction.strftime("%B") for t in transactions
        ],
        GroupByOption.year: [str(t.date_of_transaction.year) for t in transactions],
        GroupByOption.account: [
            ts_lookup[t.transaction_source_id].name for t in transactions
        ],
        GroupByOption.budget: [],
    }

    for key, value in val.items():
        val[key] = list(set(value))

    return val


@router.post(
    "/demo_aggregated",
    response_model=AggregatedTransactions,
)
def get_demo_aggregated_transactions(
    current_filter: FilterData | None = None,
) -> AggregatedTransactions:
    if not current_filter:
        current_filter = FilterData()
    demo_data = get_demo_data()
    transactions = demo_data.transactions

    category_lookup = {c.id: c for c in demo_data.categories}

    ts_lookup = {ts.id: ts for ts in demo_data.sources}

    ts_lookup_by_name = {ts.name: ts.id for ts in demo_data.sources}
    category_lookup_by_name = {c.name: c.id for c in demo_data.categories}

    category_filter = current_filter.lookup.get(GroupByOption.category)
    if category_filter and category_filter.specifics:
        category_args = [
            c for c in category_filter.specifics if c.value in category_lookup_by_name
        ]
    else:
        category_args = None

    transaction_source_filter = current_filter.lookup.get(GroupByOption.account)
    if transaction_source_filter and transaction_source_filter.specifics:
        transaction_source_args = [
            ts
            for ts in transaction_source_filter.specifics
            if ts.value in ts_lookup_by_name
        ]
    else:
        transaction_source_args = None

    calls = [
        (apply_month_filter, current_filter.lookup.get(GroupByOption.month)),
        (apply_year_filter, current_filter.lookup.get(GroupByOption.year)),
        (apply_category_filter, category_args),
        (apply_account_filter, transaction_source_args),
    ]

    for a_callable, arg in calls:
        if arg:
            transactions = a_callable(transactions, arg)  # type: ignore

    if not transactions:
        return AggregatedTransactions(
            groups=[],
            group_by_ordering=[],
            overall_withdrawals=0.0,
            overall_deposits=0.0,
            overall_balance=0.0,
            grouping_options_choices={},
        )

    overall_withdrawals = 0.0
    overall_deposits = 0.0

    transactions.sort(key=lambda t: t.transaction_source_id)

    overall_withdrawals = sum(t.amount for t in transactions if t.kind == "withdrawal")
    overall_deposits = sum(t.amount for t in transactions if t.kind == "deposit")

    budgets_lookup: BudgetLookup = {}

    group_by_with_hidden_removed = sorted(
        [key for key, entries in current_filter.lookup.items() if entries.visible],
        key=lambda x: current_filter.lookup[x].index,
    )

    groups = recursive_grouping(
        transactions,
        group_by_with_hidden_removed,
        category_lookup,
        ts_lookup,
        budgets_lookup,
    )

    grouping_option_choices = get_demo_grouping_options(
        demo_data.transactions, category_lookup, ts_lookup
    )

    overall_balance = overall_deposits - overall_withdrawals
    return AggregatedTransactions(
        groups=groups,
        overall_withdrawals=overall_withdrawals,
        overall_deposits=overall_deposits,
        overall_balance=overall_balance,
        grouping_options_choices=grouping_option_choices,
        group_by_ordering=group_by_with_hidden_removed,
    )
