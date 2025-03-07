from collections.abc import Callable
from datetime import datetime
from itertools import groupby
from typing import cast

from fastapi import APIRouter, Depends, Query
from sqlalchemy import func
from sqlalchemy.orm import Query as SqlQuery

from app.db import (
    Session,
    get_current_user,
    get_db,
)
from app.local_types import (
    AggregatedGroup,
    AggregatedTransactions,
    CategoryOut,
    GroupByOption,
    TransactionEdit,
    TransactionOut,
)
from app.models import (
    Category,
    CategoryId,
    Transaction,
    TransactionSource,
    TransactionSourceId,
    User,
)

router = APIRouter(prefix="/transactions", tags=["transactions"])


CategoryLookup = dict[CategoryId, Category]
AccountLookup = dict[TransactionSourceId, TransactionSource]

GroupByKeyFunc = Callable[[Transaction, CategoryLookup, AccountLookup], str]
SortFunc = Callable[[Transaction, CategoryLookup], str | datetime]
GroupByNameFunc = Callable[[str], str]
GroupByIdFunc = Callable[[str], str]


def get_stylized_name_lookup(session: Session, user: User) -> dict[int, str]:
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


def get_account_key(
    transaction: Transaction, _lookup: CategoryLookup, account_lookup: AccountLookup
) -> str:
    return account_lookup[transaction.transaction_source_id].name


def get_category_key(
    transaction: Transaction, lookup: CategoryLookup, _account_lookup: AccountLookup
) -> str:
    return lookup[transaction.category_id].name


def get_month_key(
    transaction: Transaction, _lookup: CategoryLookup, _account_lookup: AccountLookup
) -> str:
    return transaction.date_of_transaction.strftime("%B %Y")


def get_year_key(
    transaction: Transaction, _lookup: CategoryLookup, _account_lookup: AccountLookup
) -> str:
    return str(transaction.date_of_transaction.year)


def get_category_sort(transaction: Transaction, lookup: CategoryLookup) -> str:
    return lookup[transaction.category_id].name


def get_month_sort(transaction: Transaction, _lookup: CategoryLookup) -> datetime:
    return transaction.date_of_transaction


def get_year_sort(transaction: Transaction, _lookup: CategoryLookup) -> datetime:
    return transaction.date_of_transaction


def get_account_sort(transaction: Transaction, _lookup: CategoryLookup) -> str:
    return str(transaction.transaction_source_id)


def id(key: str) -> str:
    return key


group_by_key_funcs: dict[GroupByOption, GroupByKeyFunc] = {
    GroupByOption.account: get_account_key,
    GroupByOption.category: get_category_key,
    GroupByOption.month: get_month_key,
    GroupByOption.year: get_year_key,
}

sort_funcs: dict[GroupByOption, SortFunc] = {
    GroupByOption.category: get_category_sort,
    GroupByOption.month: get_month_sort,
    GroupByOption.year: get_year_sort,
    GroupByOption.account: get_account_sort,
}


group_by_name_funcs: dict[GroupByOption, GroupByNameFunc] = {
    GroupByOption.category: id,
    GroupByOption.month: id,
    GroupByOption.year: id,
    GroupByOption.account: id,
}

group_by_id_funcs: dict[GroupByOption, GroupByIdFunc] = {
    GroupByOption.category: id,
    GroupByOption.month: id,
    GroupByOption.year: id,
    GroupByOption.account: id,
}


def recursive_group(
    txns: list[Transaction],
    group_options: list[GroupByOption],
    category_lookup: CategoryLookup,
    account_lookup: AccountLookup,
) -> list[AggregatedGroup]:
    if not group_options:
        total_withdrawals = sum(t.amount for t in txns if t.kind == "withdrawal")
        total_deposits = sum(t.amount for t in txns if t.kind == "deposit")
        return [
            AggregatedGroup(
                group_id="all",
                group_name="All",
                groupby_kind=None,
                total_withdrawals=total_withdrawals,
                total_deposits=total_deposits,
                total_balance=total_deposits - total_withdrawals,
                transactions=[TransactionOut.model_validate(t) for t in txns],
                subgroups=[],
            )
        ]

    current = group_options[0]

    def key_fn(txn: Transaction) -> str:
        return group_by_key_funcs[current](txn, category_lookup, account_lookup)

    def sort_fn(txn: Transaction) -> str | datetime:
        return sort_funcs[current](txn, category_lookup)

    name_fn = group_by_name_funcs[current]
    id_fn = group_by_id_funcs[current]

    txns.sort(key=sort_fn)
    groups = []
    for key, group_iter in groupby(txns, key=key_fn):
        group_list = list(group_iter)
        group_withdrawals = sum(t.amount for t in group_list if t.kind == "withdrawal")
        group_deposits = sum(t.amount for t in group_list if t.kind == "deposit")
        balance = group_deposits - group_withdrawals
        group_id = id_fn(key)
        group_name = name_fn(key)

        if len(group_options) > 1:
            subgroups = recursive_group(
                group_list, group_options[1:], category_lookup, account_lookup
            )
            groups.append(
                AggregatedGroup(
                    group_id=group_id,
                    group_name=group_name,
                    groupby_kind=current,
                    total_withdrawals=group_withdrawals,
                    total_deposits=group_deposits,
                    total_balance=balance,
                    subgroups=subgroups,
                    transactions=[],
                )
            )
        else:
            groups.append(
                AggregatedGroup(
                    group_id=group_id,
                    group_name=group_name,
                    groupby_kind=current,
                    total_withdrawals=group_withdrawals,
                    total_deposits=group_deposits,
                    total_balance=balance,
                    subgroups=[],
                    transactions=[TransactionOut.model_validate(t) for t in group_list],
                )
            )
    return groups


@router.get(
    "/",
    dependencies=[Depends(get_current_user)],
    response_model=list[TransactionOut],
)
def get_transactions(
    session: Session = Depends(get_db), user: User = Depends(get_current_user)
) -> list[Transaction]:
    val = session.query(Transaction).filter(Transaction.user_id == user.id).all()
    return val


def build_grouping_option_choices(
    session: Session, user: User
) -> dict[GroupByOption, list[str]]:
    all_dates = (
        session.query(Transaction.date_of_transaction)
        .filter(Transaction.user_id == user.id)
        .distinct()
        .order_by(Transaction.date_of_transaction)
        .all()
    )
    all_categories = (
        session.query(Category.name)
        .filter(Category.user_id == user.id)
        .distinct()
        .order_by(Category.name)
        .all()
    )
    all_accounts = (
        session.query(TransactionSource.name)
        .filter(TransactionSource.user_id == user.id)
        .distinct()
        .order_by(TransactionSource.name)
        .all()
    )

    return {
        GroupByOption.year: list(
            {str(row.date_of_transaction.year) for row in all_dates}
        ),
        GroupByOption.month: list(
            {str(row.date_of_transaction.strftime("%B")) for row in all_dates}
        ),
        GroupByOption.category: list({category.name for category in all_categories}),
        GroupByOption.account: list({account.name for account in all_accounts}),
    }


def apply_category_filter(
    transactions: SqlQuery[Transaction],
    categories: list[str],
) -> SqlQuery[Transaction]:
    return transactions.join(Category, Category.id == Transaction.category_id).filter(
        Category.name.in_(categories)
    )


def apply_account_filter(
    transactions: SqlQuery[Transaction],
    accounts: list[str],
) -> SqlQuery[Transaction]:
    return transactions.join(
        TransactionSource, TransactionSource.id == Transaction.transaction_source_id
    ).filter(TransactionSource.name.in_(accounts))


def apply_year_filter(
    transactions: SqlQuery[Transaction],
    years: list[str],
) -> SqlQuery[Transaction]:
    return transactions.filter(
        func.extract("year", Transaction.date_of_transaction).in_(list(map(int, years)))
    )


def apply_month_filter(
    transactions: SqlQuery[Transaction],
    months: list[str],
) -> SqlQuery[Transaction]:
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
    return transactions.filter(
        func.extract("month", Transaction.date_of_transaction).in_(month_numbers)
    )


@router.get(
    "/aggregated",
    dependencies=[Depends(get_current_user)],
    response_model=AggregatedTransactions,
)
def get_aggregated_transactions(
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
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> AggregatedTransactions:
    transactions_query = session.query(Transaction).filter(
        Transaction.user_id == user.id
    )

    calls: list[
        tuple[
            Callable[[SqlQuery[Transaction], list[str]], SqlQuery[Transaction]],
            list[str] | None,
        ]
    ] = [
        (apply_month_filter, months),
        (apply_year_filter, years),
        (apply_category_filter, categories),
        (apply_account_filter, accounts),
    ]

    for a_callable, arg in calls:
        if arg and len(arg) > 0:
            transactions_query = a_callable(transactions_query, arg)

    transactions = transactions_query.all()

    if not transactions:
        return AggregatedTransactions(
            groups=[],
            overall_withdrawals=0.0,
            overall_deposits=0.0,
            overall_balance=0.0,
            grouping_options_choices={},
        )

    if GroupByOption.category in group_by:
        category_ids = {txn.category_id for txn in transactions}
        category_lookup = {
            c.id: c
            for c in session.query(Category).filter(Category.id.in_(category_ids))
        }
    else:
        category_lookup = {}

    ts_ids = {txn.transaction_source_id for txn in transactions}
    ts_lookup = {
        ts.id: ts
        for ts in session.query(TransactionSource).filter(
            TransactionSource.id.in_(ts_ids)
        )
    }

    overall_withdrawals = 0.0
    overall_deposits = 0.0

    transactions.sort(key=lambda t: t.transaction_source_id)

    overall_withdrawals = sum(t.amount for t in transactions if t.kind == "withdrawal")
    overall_deposits = sum(t.amount for t in transactions if t.kind == "deposit")

    groups = recursive_group(transactions, group_by, category_lookup, ts_lookup)

    grouping_option_choices = build_grouping_option_choices(session, user)

    overall_balance = overall_deposits - overall_withdrawals
    return AggregatedTransactions(
        groups=groups,
        overall_withdrawals=overall_withdrawals,
        overall_deposits=overall_deposits,
        overall_balance=overall_balance,
        grouping_options_choices=grouping_option_choices,
    )


@router.put(
    "/{transaction_id}",
    response_model=TransactionOut,
)
def update_transaction(
    transaction: TransactionEdit,
    user: User = Depends(get_current_user),
    session: Session = Depends(get_db),
) -> Transaction:
    transaction_db = (
        session.query(Transaction)
        .filter(Transaction.id == transaction.id, Transaction.user_id == user.id)
        .one()
    )

    transaction_db.amount = transaction.amount
    transaction_db.description = transaction.description
    transaction_db.date_of_transaction = transaction.date_of_transaction
    transaction_db.kind = transaction.kind
    transaction_db.category_id = cast(CategoryId, transaction.category_id)

    session.commit()
    return transaction_db


@router.get(
    "list_categories/{transaction_id}",
    response_model=list[CategoryOut],
)
def list_categories(
    transaction_id: int,
    user: User = Depends(get_current_user),
    session: Session = Depends(get_db),
) -> list[CategoryOut]:
    categories_query = session.query(Category).filter(
        Category.user_id == user.id,
    )

    transaction_db = (
        session.query(Transaction)
        .filter(Transaction.id == transaction_id, Transaction.user_id == user.id)
        .filter(~Transaction.archived)
        .one()
    )
    categories_query = categories_query.filter(
        Category.source_id == transaction_db.transaction_source_id
    )

    stylized_name_lookup = get_stylized_name_lookup(session, user)

    return [
        CategoryOut(name=category.name, stylized_name=stylized_name_lookup[category.id], id=category.id, archived=category.archived, source_id=category.source_id)
        for category in categories_query.all()
    ]


@router.get(
    "list_all_categories",
    response_model=list[CategoryOut],
)
def list_all_categories(
    user: User = Depends(get_current_user),
    session: Session = Depends(get_db),
) -> list[CategoryOut]:
    categories_query = session.query(Category).filter(
        Category.user_id == user.id,
    )

    stylized_name_lookup = get_stylized_name_lookup(session, user)

    return [
        CategoryOut(name=category.name, stylized_name=stylized_name_lookup[category.id], id=category.id, archived=category.archived, source_id=category.source_id)
        for category in categories_query.all()
    ]
