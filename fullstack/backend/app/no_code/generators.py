from collections import defaultdict
from datetime import timedelta, datetime
import random
import enum
from decimal import Decimal

from pydantic import BaseModel
from sqlalchemy import func

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
from app.no_code.transformations import KeyValuePair


@pipeline_step(
    return_type=list[KeyValuePair],
    passed_value=None,
)
def total_amount_per_category(data: PipelineStart) -> list[KeyValuePair]:
    txs = data.session.query(func.sum(Transaction.amount), Category.name).join(
        Category, Transaction.category_id == Category.id
    )
    txs = txs.filter(Transaction.user_id == data.user.id).group_by(Category.name)

    return [KeyValuePair(key=cat, value=Decimal(amount)) for amount, cat in txs.all()]


@pipeline_step(
    return_type=list[NoCodeTransaction],
    passed_value=None,
)
def first_n_transactions(
    data: PipelineStart,
    n: SelectOption,
    account_id: SelectOption | None,
    page: SelectOption = SelectOption(key="1", value="1"),
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
        .offset((int(page.key) - 1) * int(n.key))
        .limit(int(n.key))
    )

    val = [
        NoCodeTransaction(
            category_name=cat.name,
            amount=tx.amount,
            kind=tx.kind,
            description=tx.description,
            date_of_transaction=tx.date_of_transaction,
        )
        for tx, cat in txs.all()
    ]
    return val


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
    trend: Decimal


@pipeline_step(
    return_type=ResultWithTrend | None,
    passed_value=None,
)
def account_balance(
    data: PipelineStart, account_id: SelectOption
) -> ResultWithTrend | None:
    plaid_account = (
        data.session.query(PlaidAccountBalance)
        .join(TransactionSource, PlaidAccountBalance.transaction_source_id == TransactionSource.id)
        .filter(PlaidAccount.user_id == data.user.id)
        .filter(TransactionSource.id == int(account_id.key))
        .order_by(PlaidAccountBalance.timestamp.desc())
        .limit(10)
        .all()
    )

    if not plaid_account:
        return None

    if len(plaid_account) >= 2:
        trend = determine_percent_change(
            Decimal(plaid_account[1].balance), Decimal(plaid_account[0].balance)
        )
    else:
        trend = Decimal(0)

    return ResultWithTrend(
        result=round(Decimal(plaid_account[0].balance), 2),
        unit=Unit.DOLLAR,
        trend=trend,
        trend_data=TrendData(
            values=[TrendValue(value=Decimal(val.balance)) for val in plaid_account],
            color="orange",
        ),
    )


def determine_percent_change(start: Decimal, end: Decimal) -> Decimal:
    if start == 0:
        return Decimal(0)
    return round((end - start) / start * 100, 2)


@pipeline_step(
    return_type=ResultWithTrend | None,
    passed_value=None,
)
def all_account_balances(data: PipelineStart) -> ResultWithTrend | None:
    # select all transaction sources for the user
    accounts = (
        data.session.query(TransactionSource)
        .filter(TransactionSource.user_id == data.user.id)
        .all()
    )
    if not accounts:
        return None

    # pull 10 records of balance for every account the user has
    plaid_account_balances = defaultdict(list)
    for account in accounts:
        plaid_account_balances[account.id] = (
            data.session.query(PlaidAccountBalance)
            .filter(PlaidAccountBalance.transaction_source_id == account.id)
            .order_by(PlaidAccountBalance.timestamp.desc())
            .limit(10)
            .all()
        )

    # total the most recent entry from each account
    net_worth = Decimal(0)
    all_transactions = []
    for _account_id, balance_entries in plaid_account_balances.items():
        if not balance_entries:
            continue
        net_worth += Decimal(balance_entries[0].balance)
        all_transactions.extend(balance_entries)

    # sort the transactions by timestamp
    all_transactions.sort(key=lambda x: x.timestamp)

    # some function to track net worth over time across all accounts?

    latest_balances = defaultdict(lambda: Decimal(0))
    net_worth_history = []
    for update in all_transactions:
        latest_balances[update.plaid_account_id] = Decimal(update.balance)
        # Net worth at this timestamp is the sum of all latest balances
        net_worth_history_point = sum(latest_balances.values())
        net_worth_history.append((update.timestamp, net_worth_history_point))

    result = round(Decimal(net_worth), 2)

    if len(net_worth_history) >= 2:
        trend = determine_percent_change(
            Decimal(net_worth_history[-2][1]), Decimal(net_worth_history[-1][1])
        )
    else:
        trend = Decimal(0)

    return ResultWithTrend(
        result=result,
        unit=Unit.DOLLAR,
        trend=trend,
        trend_data=TrendData(
            values=[
                TrendValue(value=Decimal(val)) for _timestamp, val in net_worth_history
            ],
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
        trend=determine_percent_change(Decimal(1000), Decimal(1000)),
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
    data: PipelineStart, account_id: SelectOption, time_unit: SelectOption
) -> ResultWithTrend | None:
    if time_unit.key == "week":
        DESIRED_DATA = 7
    elif time_unit.key == "month":
        DESIRED_DATA = 30
    elif time_unit.key == "year":
        DESIRED_DATA = 365

    now = datetime.now()
    current_cutoff = now - timedelta(days=DESIRED_DATA)
    earliest_date = now - timedelta(days=DESIRED_DATA * 2)

    transactions = (
        data.session.query(Transaction)
        .filter(Transaction.transaction_source_id == account_id.key)
        .filter(Transaction.user_id == data.user.id)
        .filter(Transaction.date_of_transaction >= earliest_date)
        .order_by(Transaction.date_of_transaction)
        .all()
    )

    previous_amount = Decimal(0)
    this_amount = Decimal(0)
    for tx in transactions:
        to_add = tx.amount if tx.kind == TransactionKind.deposit else -tx.amount

        if tx.date_of_transaction > current_cutoff:
            this_amount += Decimal(to_add)
        else:
            previous_amount += Decimal(to_add)

    return ResultWithTrend(
        result=Decimal(this_amount),
        unit=Unit.DOLLAR,
        trend=determine_percent_change(previous_amount, this_amount),
        trend_data=TrendData(
            values=[TrendValue(value=previous_amount), TrendValue(value=this_amount)],
            color="blue",
        ),
    )
