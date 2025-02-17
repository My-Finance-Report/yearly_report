from typing import Callable, Union, overload
from fastapi import APIRouter, Depends, Query
from itertools import groupby

from enum import Enum

from app.api.deps import (
    get_current_user,
)
from app.db import Session, get_db
from app.models import (
    Category,
    Transaction,
    TransactionSource,
    User,
)

from app.local_types import (
    AggregatedGroup,
    AggregatedTransactions,
    TransactionOut,
    TransactionSourceGroup
)

router = APIRouter(prefix="/transactions", tags=["transactions"])


class GroupByOption(str, Enum):
    category = "category"
    month = "month"
    year = "year"

CategoryLookup = dict[int, Category]

GroupKeyType = int | tuple[int, int]
GroupByKeyFunc = Callable[[Transaction], GroupKeyType]
GroupByNameFunc = Callable[[GroupKeyType, CategoryLookup], str]
GroupByIdFunc = Callable[[GroupKeyType], int]

# Helper functions
def get_category_key(transaction: Transaction) -> int:
    return transaction.category_id

def get_month_key(transaction: Transaction) -> tuple[int, int]:
    return (transaction.date_of_transaction.year, transaction.date_of_transaction.month)

def get_year_key(transaction: Transaction) -> int:
    return transaction.date_of_transaction.year

def get_category_name(key: int, lookup: CategoryLookup) -> str:
    return lookup[key].name if key in lookup else str(key)

def get_month_name(key: tuple[int, int], _: CategoryLookup) -> str:
    return f"{key[0]}-{key[1]:02d}"

def get_year_name(key: int, _: CategoryLookup) -> str:
    return str(key)

# Overloaded function for correct type checking
@overload
def get_category_id(key: int) -> int: ...
@overload
def get_category_id(key: tuple[int, int]) -> int: ...

def get_category_id(key: Union[int, tuple[int, int]]) -> int:
    if isinstance(key, tuple):
        raise ValueError("Category ID should not be a tuple")
    return key

@overload
def get_month_id(key: tuple[int, int]) -> int: ...
@overload
def get_month_id(key: int) -> int: ...

def get_month_id(key: Union[int, tuple[int, int]]) -> int:
    if isinstance(key, int):
        raise ValueError("Month ID should be a tuple")
    return key[0] * 100 + key[1]  # Converts (YYYY, MM) to YYYYMM integer

def get_year_id(key: int) -> int:
    return key

# Mapping from GroupByOption to functions
group_by_key_funcs: dict[GroupByOption, GroupByKeyFunc] = {
    GroupByOption.category: get_category_key,
    GroupByOption.month: get_month_key,
    GroupByOption.year: get_year_key,
}

# TODO update to make sure this is actually typed correctly

group_by_name_funcs: dict[GroupByOption, GroupByNameFunc] = {
    GroupByOption.category: get_category_name, #type: ignore
    GroupByOption.month: get_month_name,#type: ignore
    GroupByOption.year: get_year_name,#type: ignore
}

group_by_id_funcs: dict[GroupByOption, GroupByIdFunc] = {
    GroupByOption.category: get_category_id,#type: ignore
    GroupByOption.month: get_month_id,#type: ignore
    GroupByOption.year: get_year_id,#type: ignore
}

def recursive_group(
    txns: list[Transaction],
    group_options: list[GroupByOption],
    category_lookup: CategoryLookup,
) -> list[AggregatedGroup]:
    if not group_options:
        # Should not happen normally; if no grouping remains, return a single leaf group.
        total_withdrawals = sum(t.amount for t in txns if t.kind == "withdrawal")
        total_deposits = sum(t.amount for t in txns if t.kind == "deposit")
        return [
            AggregatedGroup(
                group_id="all",
                group_name="All",
                total_withdrawals=total_withdrawals,
                total_deposits=total_deposits,
                total_balance=total_deposits - total_withdrawals,
                transactions=[TransactionOut.model_validate(t) for t in txns],
                subgroups=[],
            )
        ]

    current = group_options[0]
    key_fn = group_by_key_funcs[current]
    name_fn = group_by_name_funcs[current]
    id_fn = group_by_id_funcs[current]

    # Sort by current grouping key.
    txns.sort(key=key_fn)
    groups = []
    for key, group_iter in groupby(txns, key=key_fn):
        group_list = list(group_iter)
        group_withdrawals = sum(t.amount for t in group_list if t.kind == "withdrawal")
        group_deposits = sum(t.amount for t in group_list if t.kind == "deposit")
        balance = group_deposits - group_withdrawals
        group_id = id_fn(key)
        group_name = name_fn(key, category_lookup if current == GroupByOption.category else {})

        if len(group_options) > 1:
            subgroups = recursive_group(group_list, group_options[1:], category_lookup)
            groups.append(
                AggregatedGroup(
                    group_id=group_id,
                    group_name=group_name,
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
def get_transactions(session:Session =Depends(get_db), user: User = Depends(get_current_user)) -> list[Transaction]:
    val =  session.query(Transaction).filter(Transaction.user_id == user.id).all()
    return val
    


@router.get(
    "/aggregated",
    dependencies=[Depends(get_current_user)],
    response_model=AggregatedTransactions,
)
def get_aggregated_transactions(
    group_by: list[GroupByOption] = Query([GroupByOption.category], description="List of grouping options in order (e.g. category, month)"),
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> AggregatedTransactions:
    transactions = session.query(Transaction).filter(Transaction.user_id == user.id).all()
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
            c.id: c for c in session.query(Category).filter(Category.id.in_(category_ids))
        }
    else:
        category_lookup = {}

    # Build lookup for transaction sources.
    ts_ids = {txn.transaction_source_id for txn in transactions}
    ts_lookup = {
        ts.id: ts for ts in session.query(TransactionSource).filter(TransactionSource.id.in_(ts_ids))
    }

    overall_withdrawals = 0.0
    overall_deposits = 0.0
    ts_groups = []  # Will hold TransactionSourceGroup objects

    # Group transactions by transaction_source_id.
    transactions.sort(key=lambda t: t.transaction_source_id)
    for ts_id, ts_txns_iter in groupby(transactions, key=lambda t: t.transaction_source_id):
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