import random
from decimal import Decimal
from typing import cast

from pydantic import BaseModel

from app.models import Category, PlaidSyncLog, Transaction, TransactionSource
from app.no_code.decoration import pipeline_step
from app.no_code.step import Kwargs
from app.schemas.no_code import NoCodeTransaction, PipelineStart, SelectOption


@pipeline_step(
    return_type=list[NoCodeTransaction],
    passed_value=None,
)
def first_n_transactions(
    data: PipelineStart, n: int, account_id: SelectOption 
) -> list[NoCodeTransaction]:

    txs = data.session.query(Transaction, Category).join(
        Category, Transaction.category_id == Category.id
    )

    if account_id:
        txs = txs.join(
            TransactionSource, Transaction.transaction_source_id == TransactionSource.id
        ).filter(TransactionSource.id == account_id.key)

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
            kind=tx.kind,
            description=tx.description,
            date_of_transaction=tx.date_of_transaction,
        )
        for tx, cat in txs.all()
    ]

@pipeline_step(
    return_type=str | None,
    passed_value=None,
)
def account_name(data: PipelineStart,account_id: SelectOption ) -> str | None:

    query = (
        data.session.query(TransactionSource.name)
        .filter(TransactionSource.user_id == data.user.id)
        .filter(TransactionSource.id == account_id.key)
        .one_or_none()
    )
    return query.name if query else None

@pipeline_step(
    return_type=str | None,
    passed_value=None,
)
def plaid_enabled(data: PipelineStart,account_id: SelectOption ) -> str | None:

    query = (
        data.session.query(TransactionSource.plaid_account_id)
        .filter(TransactionSource.user_id == data.user.id)
        .filter(TransactionSource.id == account_id.key)
        .one_or_none()
    )
    if not query:
        return None
    
    if query.plaid_account_id is not None:
        return "Plaid Enabled"
    
    else: 
        return "Powered by Statements"

@pipeline_step(
    return_type=str | None,
    passed_value=None,
)
def last_plaid_sync(data: PipelineStart,account_id: SelectOption ) -> str | None:

    db_source = (
        data.session.query(TransactionSource)
        .filter(TransactionSource.id == account_id.key, TransactionSource.user_id == data.user.id)
        .first()
    )

    if not db_source:
        return "No plaid sync"

    if not db_source.plaid_account_id:
        return "No plaid sync"

    sync_logs = (
        data.session.query(PlaidSyncLog)
        .filter(
            PlaidSyncLog.user_id == data.user.id,
            PlaidSyncLog.plaid_account_id == db_source.plaid_account_id,
        )
        .order_by(PlaidSyncLog.created_at.desc())
        .limit(1)
        .all()
    )

    return sync_logs[0].created_at.strftime("%Y-%m-%d") if sync_logs else "No previous sync"

class ResultWithTrend(BaseModel):
    result: Decimal
    trend: Decimal

@pipeline_step(
    return_type=ResultWithTrend | None,
    passed_value=None,
)
def account_balance(data: PipelineStart,account_id: SelectOption ) -> ResultWithTrend | None:
    data.session.query(TransactionSource.name).filter(
        TransactionSource.id == account_id.key
    ).filter(TransactionSource.user_id == data.user.id).one_or_none()

    return ResultWithTrend(result=Decimal(1000), trend=Decimal(100))

@pipeline_step(
    return_type=ResultWithTrend | None,
    passed_value=None,
)
def account_interest(data: PipelineStart,account_id: SelectOption ) -> ResultWithTrend | None:
    data.session.query(TransactionSource.name).filter(
        TransactionSource.id == account_id.key
    ).filter(TransactionSource.user_id == data.user.id).one_or_none()

    return ResultWithTrend(result=Decimal(1000), trend=Decimal(100))

@pipeline_step(
    return_type=ResultWithTrend | None,
    passed_value=None,
)
def account_throughput(data: PipelineStart,account_id: SelectOption ) -> ResultWithTrend | None:
    data.session.query(TransactionSource.name).filter(
        TransactionSource.id == account_id.key
    ).filter(TransactionSource.user_id == data.user.id).one_or_none()

    return ResultWithTrend(result=Decimal(1000), trend=Decimal(100))
