from collections.abc import Callable
from datetime import datetime
from itertools import groupby
from typing import cast

from fastapi import APIRouter, Depends, Query

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
    TransactionSourceGroup,
)
from app.models import (
    Category,
    CategoryId,
    Transaction,
    TransactionSource,
    User,
)

router = APIRouter(prefix="/transactions", tags=["transactions"])


CategoryLookup = dict[CategoryId, Category]

GroupByKeyFunc = Callable[[Transaction, CategoryLookup], str]
SortFunc = Callable[[Transaction, CategoryLookup], str | datetime]
GroupByNameFunc = Callable[[str], str]
GroupByIdFunc = Callable[[str], str]


# Helper functions
def get_category_key(transaction: Transaction, lookup: CategoryLookup) -> str:
    return lookup[transaction.category_id].name


def get_month_key(transaction: Transaction, _lookup: CategoryLookup) -> str:
    return transaction.date_of_transaction.strftime("%B %Y")


def get_year_key(transaction: Transaction, _lookup: CategoryLookup) -> str:
    return str(transaction.date_of_transaction.year)


def get_category_sort(transaction: Transaction, lookup: CategoryLookup) -> str:
    return lookup[transaction.category_id].name


def get_month_sort(transaction: Transaction, _lookup: CategoryLookup) -> datetime:
    return transaction.date_of_transaction


def get_year_sort(transaction: Transaction, _lookup: CategoryLookup) -> datetime:
    return transaction.date_of_transaction


def get_category_name(key: str) -> str:
    return key


def get_month_name(key: str) -> str:
    return key


def get_year_name(key: str) -> str:
    return key


def get_category_id(key: str) -> str:
    return key


def get_month_id(key: str) -> str:
    return key


def get_year_id(key: str) -> str:
    return key


# Mapping from GroupByOption to functions
group_by_key_funcs: dict[GroupByOption, GroupByKeyFunc] = {
    GroupByOption.category: get_category_key,
    GroupByOption.month: get_month_key,
    GroupByOption.year: get_year_key,
}

sort_funcs: dict[GroupByOption, SortFunc] = {
    GroupByOption.category: get_category_sort,
    GroupByOption.month: get_month_sort,
    GroupByOption.year: get_year_sort,
}


group_by_name_funcs: dict[GroupByOption, GroupByNameFunc] = {
    GroupByOption.category: get_category_name,
    GroupByOption.month: get_month_name,
    GroupByOption.year: get_year_name,
}

group_by_id_funcs: dict[GroupByOption, GroupByIdFunc] = {
    GroupByOption.category: get_category_id,
    GroupByOption.month: get_month_id,
    GroupByOption.year: get_year_id,
}


def recursive_group(
    txns: list[Transaction],
    group_options: list[GroupByOption],
    category_lookup: CategoryLookup,
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
        return group_by_key_funcs[current](txn, category_lookup)

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
            subgroups = recursive_group(group_list, group_options[1:], category_lookup)
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
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> AggregatedTransactions:
    transactions = (
        session.query(Transaction).filter(Transaction.user_id == user.id).all()
    )
    if not transactions:
        return AggregatedTransactions(
            groups=[],
            overall_withdrawals=0.0,
            overall_deposits=0.0,
            overall_balance=0.0,
        )

    # Build lookup for categories if grouping by category is requested.
    if GroupByOption.category in group_by:
        category_ids = {txn.category_id for txn in transactions}
        category_lookup = {
            c.id: c
            for c in session.query(Category).filter(Category.id.in_(category_ids))
        }
    else:
        category_lookup = {}

    # Build lookup for transaction sources.
    ts_ids = {txn.transaction_source_id for txn in transactions}
    ts_lookup = {
        ts.id: ts
        for ts in session.query(TransactionSource).filter(
            TransactionSource.id.in_(ts_ids)
        )
    }

    overall_withdrawals = 0.0
    overall_deposits = 0.0
    ts_groups = []  # Will hold TransactionSourceGroup objects

    # Group transactions by transaction_source_id.
    transactions.sort(key=lambda t: t.transaction_source_id)
    for ts_id, ts_txns_iter in groupby(
        transactions, key=lambda t: t.transaction_source_id
    ):
        ts_txns = list(ts_txns_iter)
        # Compute source-level totals.
        ts_withdrawals = sum(t.amount for t in ts_txns if t.kind == "withdrawal")
        ts_deposits = sum(t.amount for t in ts_txns if t.kind == "deposit")
        overall_withdrawals += ts_withdrawals
        overall_deposits += ts_deposits

        # Use the recursive function to group by the provided options.
        groups = recursive_group(ts_txns, group_by, category_lookup)

        ts_obj = ts_lookup[ts_id]
        ts_groups.append(
            TransactionSourceGroup(
                transaction_source_id=ts_obj.id,
                transaction_source_name=ts_obj.name,
                total_withdrawals=ts_withdrawals,
                total_deposits=ts_deposits,
                total_balance=ts_deposits - ts_withdrawals,
                groups=groups,
            )
        )

    overall_balance = overall_deposits - overall_withdrawals
    return AggregatedTransactions(
        groups=ts_groups,
        overall_withdrawals=overall_withdrawals,
        overall_deposits=overall_deposits,
        overall_balance=overall_balance,
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
) -> list[Category]:
    transaction_db = (
        session.query(Transaction)
        .filter(Transaction.id == transaction_id, Transaction.user_id == user.id)
        .one()
    )
    categories = (
        session.query(Category)
        .filter(
            Category.user_id == user.id,
            Category.source_id == transaction_db.transaction_source_id,
        )
        .all()
    )

    return categories
