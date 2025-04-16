import random
from decimal import Decimal
from typing import cast

from app.models import Category, Transaction, TransactionSource
from app.no_code.decoration import Arg, pipeline_step
from app.no_code.functions import make_account_choices
from app.no_code.step import Kwargs
from app.schemas.no_code import NoCodeTransaction, ParameterType, PipelineStart


@pipeline_step(
    expected_kwargs=[
        Arg(name="n", type=ParameterType.INT),
        Arg(
            name="account_id",
            type=ParameterType.SELECT,
            options_generator=make_account_choices,
        ),
    ],
    return_type=list[NoCodeTransaction],
    passed_value=None,
)
def first_n_transactions(
    data: PipelineStart, kwargs: Kwargs
) -> list[NoCodeTransaction]:
    n = cast(int, kwargs["n"])
    account_id = kwargs.get("account_id")

    txs = data.session.query(Transaction, Category).join(
        Category, Transaction.category_id == Category.id
    )

    if account_id:
        txs = txs.join(
            TransactionSource, Transaction.transaction_source_id == TransactionSource.id
        ).filter(TransactionSource.id == int(account_id))

    txs = (
        txs.filter(Transaction.user_id == data.user.id)
        .order_by(Transaction.date_of_transaction.desc())
        .limit(n)
    )

    return [
        NoCodeTransaction(
            id=tx.id,
            category_name=cat.name,
            amount=tx.amount,
            description=tx.description,
            date=tx.date_of_transaction.strftime("%m/%d/%Y"),
        )
        for tx, cat in txs.all()
    ]


@pipeline_step(
    expected_kwargs=[Arg(name="id", type=ParameterType.INT)],
    return_type=str | None,
    passed_value=None,
)
def account_name(data: PipelineStart, kwargs: Kwargs) -> str | None:
    id = cast(int, kwargs["id"])

    query = (
        data.session.query(TransactionSource.name)
        .filter(TransactionSource.user_id == data.user.id)
        .filter(TransactionSource.id == id)
        .one_or_none()
    )
    return query.name if query else None


@pipeline_step(
    expected_kwargs=[Arg(name="id", type=ParameterType.INT)],
    return_type=Decimal | None,
    passed_value=None,
)
def account_balance(data: PipelineStart, kwargs: Kwargs) -> Decimal | None:
    id = cast(int, kwargs["id"])
    data.session.query(TransactionSource.name).filter(
        TransactionSource.id == id
    ).filter(TransactionSource.user_id == data.user.id).one_or_none()

    return random.choice([Decimal(1000), Decimal(2000), Decimal(3000)])
