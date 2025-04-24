from collections import defaultdict
from datetime import timedelta, datetime
import random

from app.models import (
    PlaidAccount,
    PlaidAccountBalance,
    TransactionSource,
)
from app.no_code.decoration import pipeline_step
from app.schemas.no_code import PipelineStart, SelectOption


@pipeline_step(
    return_type=str | None,
    passed_value=None,
)
def update_balance(
    data: PipelineStart,
    account_id: SelectOption,
    balance: float,
    timestamp: str,
    submit: bool,
) -> str | None:
    if not submit:
        return None

    db_source = (
        data.session.query(TransactionSource)
        .filter(
            TransactionSource.id == account_id.key,
            TransactionSource.user_id == data.user.id,
        )
        .one()
    )

    if not db_source:
        raise ValueError("Not possible")

    if data.user.id != db_source.user_id:
        raise ValueError("Not possible")

    print("ta",timestamp)

    new_balance = PlaidAccountBalance(
        plaid_account_id=db_source.plaid_account_id,
        transaction_source_id=account_id.key,
        balance=balance,
        timestamp = datetime.strptime(timestamp, "%Y-%m-%dT%H:%M")
    )

    data.session.add(new_balance)
    data.session.commit()

    return "updated successfully!"
