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
    AggregatedTransactions,
    TransactionGroup,
    TransactionOut,
    TransactionSourceGroup
)

router = APIRouter(prefix="/transactions", tags=["transactions"])


class GroupByOption(str, Enum):
    category = "category"
    month = "month"
    year = "year"

group_by_key_funcs = {
    GroupByOption.category: lambda t: t.category_id,
    GroupByOption.month: lambda t: (t.date_of_transaction.year, t.date_of_transaction.month),
    GroupByOption.year: lambda t: t.date_of_transaction.year,
}

# Mapping from GroupByOption to a callable that returns a display name from the grouping key.
group_by_name_funcs = {
    GroupByOption.category: lambda key, category_lookup: category_lookup[key].name,
    GroupByOption.month: lambda key, _: f"{key[0]}-{key[1]:02d}",
    GroupByOption.year: lambda key, _: str(key),
}

# (Optional) Mapping from GroupByOption to a callable that returns a unique ID from the grouping key.
group_by_id_funcs = {
    GroupByOption.category: lambda key: key,
    GroupByOption.month: lambda key: int(f"{key[0]}{key[1]:02d}"),  # e.g. (2023, 2) -> 202302
    GroupByOption.year: lambda key: key,
}



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
    group_by: GroupByOption = Query(GroupByOption.category, description="Group transactions by category, month, or year"),
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> AggregatedTransactions:
    # Query all transactions for the user.
    transactions = session.query(Transaction).filter(Transaction.user_id == user.id).all()
    if not transactions:
        return AggregatedTransactions(
            groups=[],
            overall_withdrawals=0.0,
            overall_deposits=0.0,
            overall_balance=0.0,
        )

    # Build lookup for categories if needed.
    if group_by == GroupByOption.category:
        category_ids = {txn.category_id for txn in transactions}
        category_lookup = {
            c.id: c for c in session.query(Category).filter(Category.id.in_(category_ids))
        }
    else:
        # Dummy lookup; won't be used for month/year.
        category_lookup = {}

    ts_ids = {txn.transaction_source_id for txn in transactions}
    ts_lookup = {
        ts.id: ts for ts in session.query(TransactionSource).filter(TransactionSource.id.in_(ts_ids))
    }

    # Retrieve the appropriate grouping functions.
    inner_key_fn = group_by_key_funcs[group_by]
    name_fn = group_by_name_funcs[group_by]
    id_fn = group_by_id_funcs[group_by]

    # Sort transactions by transaction_source_id, then by the inner grouping key.
    transactions.sort(key=lambda t: (t.transaction_source_id, inner_key_fn(t)))

    overall_withdrawals = 0.0
    overall_deposits = 0.0
    ts_groups = []  # List of TransactionSourceGroup objects

    # Group by transaction_source_id.
    for ts_id, ts_txns_iter in groupby(transactions, key=lambda t: t.transaction_source_id):
        ts_txns = list(ts_txns_iter)
        # Within each source, sort by inner grouping key.
        ts_txns.sort(key=inner_key_fn)
        inner_groups = []
        ts_withdrawals = 0.0
        ts_deposits = 0.0

        # Group by the inner key.
        for key, group_txns_iter in groupby(ts_txns, key=inner_key_fn):
            group_txns = list(group_txns_iter)
            group_withdrawals = sum(txn.amount for txn in group_txns if txn.kind == "withdrawal")
            group_deposits = sum(txn.amount for txn in group_txns if txn.kind == "deposit")
            ts_withdrawals += group_withdrawals
            ts_deposits += group_deposits

            group_id = id_fn(key)
            group_name = name_fn(key, category_lookup)
            inner_groups.append(
                TransactionGroup(
                    category_id=group_id,  # Reusing the field names
                    category_name=group_name,
                    total_withdrawals=group_withdrawals,
                    total_deposits=group_deposits,
                    total_balance=group_deposits - group_withdrawals,
                    transactions=[TransactionOut.from_orm(txn) for txn in group_txns],
                )
            )

        overall_withdrawals += ts_withdrawals
        overall_deposits += ts_deposits

        ts_obj = ts_lookup[ts_id]
        ts_groups.append(
            TransactionSourceGroup(
                transaction_source_id=ts_obj.id,
                transaction_source_name=ts_obj.name,
                total_withdrawals=ts_withdrawals,
                total_deposits=ts_deposits,
                total_balance=ts_deposits - ts_withdrawals,
                groups=inner_groups,
            )
        )

    overall_balance = overall_deposits - overall_withdrawals

    return AggregatedTransactions(
        groups=ts_groups,
        overall_withdrawals=overall_withdrawals,
        overall_deposits=overall_deposits,
        overall_balance=overall_balance,
    )