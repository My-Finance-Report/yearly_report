from fastapi import APIRouter, Depends

from app.api.deps import (
    get_current_user,
)
from app.db import Session, get_db
from app.models import (
    Transaction,
    User,
)

from app.local_types import (
    AggregatedTransactions,
    TransactionGroup,
    TransactionOut
)

router = APIRouter(prefix="/transactions", tags=["transactions"])


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
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> AggregatedTransactions:
    # Retrieve all transactions for the user.
    transactions = session.query(Transaction).filter(Transaction.user_id == user.id).all()

    # Dictionary to hold grouped data, keyed by category_id.
    groups: dict[int, dict] = {}

    overall_withdrawals = 0.0
    overall_deposits = 0.0

    for txn in transactions:
        cat_id = txn.category_id
        if cat_id not in groups:
            groups[cat_id] = {
                "category_id": cat_id,
                "total_withdrawals": 0.0,
                "total_deposits": 0.0,
                "transactions": [],
            }

        if txn.kind == "withdrawal":
            groups[cat_id]["total_withdrawals"] += txn.amount
            overall_withdrawals += txn.amount
        elif txn.kind == "deposit":
            groups[cat_id]["total_deposits"] += txn.amount
            overall_deposits += txn.amount

        groups[cat_id]["transactions"].append(TransactionOut.from_orm(txn))

    transaction_groups = []
    for group in groups.values():
        total_balance = group["total_deposits"] - group["total_withdrawals"]
        transaction_groups.append(TransactionGroup(
            category_id=group["category_id"],
            total_withdrawals=group["total_withdrawals"],
            total_deposits=group["total_deposits"],
            total_balance=total_balance,
            transactions=group["transactions"],
        ))

    overall_balance = overall_deposits - overall_withdrawals

    return AggregatedTransactions(
        groups=transaction_groups,
        overall_withdrawals=overall_withdrawals,
        overall_deposits=overall_deposits,
        overall_balance=overall_balance,
    )