from collections import defaultdict
from datetime import timedelta, datetime
import random
import enum
from decimal import Decimal
from typing import cast

from pydantic import BaseModel

from app.models import (
    Category,
    PlaidAccount,
    PlaidAccountBalance,
    PlaidSyncLog,
    Transaction,
    TransactionKind,
    TransactionSource,
)
from app.no_code.decoration import pipeline_step
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
def account_name(data: PipelineStart, account_id: SelectOption) -> str | None:
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
def plaid_enabled(data: PipelineStart, account_id: SelectOption) -> str | None:
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
        return "Uploaded Statements"


@pipeline_step(
    return_type=str | None,
    passed_value=None,
)
def last_plaid_sync(data: PipelineStart, account_id: SelectOption) -> str | None:
    db_source = (
        data.session.query(TransactionSource)
        .filter(
            TransactionSource.id == account_id.key,
            TransactionSource.user_id == data.user.id,
        )
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

    return (
        f"Last Sync: {sync_logs[0].created_at.strftime('%Y-%m-%d')}"
        if sync_logs
        else "No previous sync"
    )


class TrendValue(BaseModel):
    value: Decimal


class TrendData(BaseModel):
    values: list[TrendValue]
    color: str


class Unit(str, enum.Enum):
    DOLLAR = "dollar"
    PERCENT = "percent"


class ResultWithTrend(BaseModel):
    result: Decimal
    unit: Unit
    trend_data: TrendData


@pipeline_step(
    return_type=ResultWithTrend | None,
    passed_value=None,
)
def account_balance(
    data: PipelineStart, account_id: SelectOption
) -> ResultWithTrend | None:
    plaid_account = (
        data.session.query(PlaidAccountBalance)
        .join(PlaidAccount, PlaidAccountBalance.plaid_account_id == PlaidAccount.id)
        .join(TransactionSource, PlaidAccount.id == TransactionSource.plaid_account_id)
        .filter(PlaidAccount.user_id == data.user.id)
        .filter(TransactionSource.id == int(account_id.key))
        .order_by(PlaidAccountBalance.timestamp.desc())
        .limit(10)
        .all()
    )

    if not plaid_account:
        return None

    return ResultWithTrend(
        result=round(Decimal(plaid_account[0].balance), 2),
        unit=Unit.DOLLAR,
        trend_data=TrendData(
            values=[TrendValue(value=Decimal(val.balance)) for val in plaid_account],
            color="orange",
        ),
    )


@pipeline_step(
    return_type=ResultWithTrend | None,
    passed_value=None,
)
def account_interest(
    data: PipelineStart, account_id: SelectOption
) -> ResultWithTrend | None:
    data.session.query(TransactionSource.name).filter(
        TransactionSource.id == account_id.key
    ).filter(TransactionSource.user_id == data.user.id).one_or_none()

    return ResultWithTrend(
        result=Decimal(1000),
        unit=Unit.PERCENT,
        trend_data=TrendData(
            values=[
                TrendValue(value=Decimal(val * 100))
                for val in random.sample(range(100, 1000), 10)
            ],
            color="green",
        ),
    )


@pipeline_step(
    return_type=ResultWithTrend | None,
    passed_value=None,
)
def account_throughput(
    data: PipelineStart, account_id: SelectOption
) -> ResultWithTrend | None:
    DESIRED_DATA = 5
    now = datetime.now()
    earliest_date = now - timedelta(days=7 * (DESIRED_DATA - 1))

    transactions = (
        data.session.query(Transaction)
        .filter(Transaction.transaction_source_id == account_id.key)
        .filter(Transaction.user_id == data.user.id)
        .filter(Transaction.date_of_transaction >= earliest_date)
        .order_by(Transaction.date_of_transaction)
        .all()
    )

    # Bucket transactions into 7-day periods
    buckets: dict[int, float] = defaultdict(float)
    for tx in transactions:
        # Calculate which bucket this transaction falls into
        days_ago = (now - tx.date_of_transaction).days
        bucket_idx = days_ago // 7
        if bucket_idx < DESIRED_DATA:
            if tx.kind == TransactionKind.withdrawal:
                buckets[bucket_idx] -= tx.amount
            elif tx.kind == TransactionKind.deposit:
                buckets[bucket_idx] += tx.amount

    # Fill in missing buckets with zero if no transactions
    values = [buckets.get(i, 0.0) for i in range(DESIRED_DATA)]

    return ResultWithTrend(
        result=Decimal(values[0]),
        unit=Unit.DOLLAR,
        trend_data=TrendData(
            values=[TrendValue(value=Decimal(val)) for val in values],
            color="blue",
        ),
    )
