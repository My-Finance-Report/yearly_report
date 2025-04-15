from decimal import Decimal
import random
from typing import cast

from app.models import Transaction, TransactionSource
from app.no_code.step import Kwargs, step
from app.schemas.no_code import NoCodeTransaction, PipelineStart


@step
def first_n_transactions(
    data: PipelineStart, kwargs: Kwargs
) -> list[NoCodeTransaction]:
    n = cast(int, kwargs["n"])
    account_id = kwargs.get("account_id")

    txs = (
        data.session.query(Transaction)
    )

    if account_id:
        txs = (
            txs.join(TransactionSource, Transaction.transaction_source_id == TransactionSource.id)
            .filter(TransactionSource.id == int(account_id))
        )

    txs = txs.filter(Transaction.user_id == data.user.id).order_by(Transaction.date_of_transaction.desc()).limit(n)

    return [
        NoCodeTransaction(id=tx.id, amount=tx.amount, description=tx.description, date=tx.date_of_transaction.strftime("%m/%d/%Y"))
        for tx in txs.all()
    ]


@step
def account_name(
    data: PipelineStart, kwargs: Kwargs
) -> str | None:
    id = cast(int, kwargs["id"])

    query = (
        data.session.query(TransactionSource.name)
        .filter(TransactionSource.user_id == data.user.id)
        .filter(TransactionSource.id == id)
        .one_or_none()
    )
    return query.name if query else None


@step
def account_balance(
    data: PipelineStart, kwargs: Kwargs
) -> Decimal | None:
    id = cast(int, kwargs["id"])

    #TODO
    
    return random.choice([Decimal(1000), Decimal(2000), Decimal(3000)])