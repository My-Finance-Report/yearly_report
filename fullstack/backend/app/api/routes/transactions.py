from fastapi import APIRouter, Depends, Query
from itertools import groupby

from enum import Enum
from operator import attrgetter

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

# Mapping from GroupByOption to a function that extracts the grouping key.
group_by_key_funcs = {
    GroupByOption.category: lambda t: t.category_id,
    GroupByOption.month: lambda t: (t.date_of_transaction.year, t.date_of_transaction.month),
    GroupByOption.year: lambda t: t.date_of_transaction.year,
}

# Mapping from GroupByOption to a function that returns a human‑readable group name.
# For category grouping, we need a lookup; for others, we ignore it.
group_by_name_funcs = {
    GroupByOption.category: lambda key, lookup: lookup[key].name if key in lookup else str(key),
    GroupByOption.month: lambda key, _: f"{key[0]}-{key[1]:02d}",
    GroupByOption.year: lambda key, _: str(key),
}

# Mapping from GroupByOption to a function that produces a unique group id.
group_by_id_funcs = {
    GroupByOption.category: lambda key: key,
    GroupByOption.month: lambda key: int(f"{key[0]}{key[1]:02d}"),  # e.g. (2023, 2) -> 202302
    GroupByOption.year: lambda key: key,
}



def recursive_group(
    txns: list,
    group_options: list[GroupByOption],
    category_lookup: dict,
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
                transactions=[TransactionOut.from_orm(t) for t in txns],
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
            # Further grouping: recursively group using the remaining options.
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
            # Leaf group: attach the list of transactions.
            groups.append(
                AggregatedGroup(
                    group_id=group_id,
                    group_name=group_name,
                    total_withdrawals=group_withdrawals,
                    total_deposits=group_deposits,
                    total_balance=balance,
                    subgroups=[],
                    transactions=[TransactionOut.from_orm(t) for t in group_list],
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