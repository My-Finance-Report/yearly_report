from typing import cast

from app.models import Transaction
from app.no_code.step import Kwargs, step
from app.schemas.no_code import NoCodeTransaction, PipelineStart


@step
def first_n_transactions(
    data: PipelineStart, kwargs: Kwargs
) -> list[NoCodeTransaction]:
    n = cast(int, kwargs["n"])

    txs = (
        data.session.query(Transaction)
        .filter(Transaction.user_id == data.user.id)
        .limit(n)
        .all()
    )
    return [
        NoCodeTransaction(id=tx.id, amount=tx.amount, description=tx.description)
        for tx in txs
    ]
