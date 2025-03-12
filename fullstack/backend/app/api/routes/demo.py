from fastapi import APIRouter, Query

from app.api.routes.demo_data import get_demo_data
from app.api.routes.transactions import BudgetLookup, recursive_group
from app.local_types import (
    AggregatedTransactions,
    GroupByOption,
)
from app.models import (
    Category,
    CategoryId,
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
    years: list[str],
) -> list[Transaction]:
    return [
        t for t in transactions if t.date_of_transaction.year in [int(y) for y in years]
    ]


def apply_month_filter(
    transactions: list[Transaction],
    months: list[str],
) -> list[Transaction]:
    # Similarly for month; pass months as integers (1 through 12)
    month_lookup = {
        "january": 1,
        "february": 2,
        "march": 3,
        "april": 4,
        "may": 5,
        "june": 6,
        "july": 7,
        "august": 8,
        "september": 9,
        "october": 10,
        "november": 11,
        "december": 12,
    }
    month_numbers = [month_lookup[m.lower()] for m in months]
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


@router.get(
    "/demo_aggregated",
    response_model=AggregatedTransactions,
)
def get_demo_aggregated_transactions(
    group_by: list[GroupByOption] = Query(
        [GroupByOption.category],
        description="List of grouping options in order (e.g. category, month)",
    ),
    years: list[str] | None = Query(
        default=None,
        description="Filter for transactions",
    ),
    months: list[str] | None = Query(
        default=None,
        description="Filter for transactions",
    ),
    categories: list[str] | None = Query(
        default=None,
        description="Filter for transactions",
    ),
    accounts: list[str] | None = Query(
        default=None,
        description="Filter for transactions",
    ),
    _budgets: list[str] | None = Query(
        default=None,
        description="Filter for transactions",
    ),
) -> AggregatedTransactions:
    demo_data = get_demo_data()
    transactions = demo_data.transactions
    category_lookup = {c.id: c for c in demo_data.categories}

    ts_lookup = {ts.id: ts for ts in demo_data.sources}

    ts_lookup_by_name = {ts.name: ts.id for ts in demo_data.sources}
    category_lookup_by_name = {c.name: c.id for c in demo_data.categories}

    calls = [
        (apply_month_filter, months),
        (apply_year_filter, years),
        (apply_category_filter, [category_lookup_by_name[c] for c in categories or []]),
        (apply_account_filter, [ts_lookup_by_name[ts] for ts in accounts or []]),
    ]

    for a_callable, arg in calls:
        if arg and len(arg) > 0:
            transactions = a_callable(transactions, arg)  # type: ignore

    if not transactions:
        return AggregatedTransactions(
            groups=[],
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

    groups = recursive_group(
        transactions, group_by, category_lookup, ts_lookup, budgets_lookup
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
    )
