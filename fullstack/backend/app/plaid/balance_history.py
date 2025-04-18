from app.models import PlaidAccount, PlaidAccountBalance, PlaidItem
from app.plaid.client import get_plaid_client
from plaid.api.plaid_api import AccountsBalanceGetRequest
from sqlalchemy.orm import Session
from datetime import datetime, timezone
from typing import Optional


def record_plaid_account_balance(
    session: Session, plaid_account: PlaidAccount
) -> Optional[PlaidAccountBalance]:
    """
    Fetch the current balance for a Plaid account and record it in the PlaidAccountBalance table.
    """
    client = get_plaid_client()
    plaid_item = (
        session.query(PlaidItem)
        .filter(PlaidItem.id == plaid_account.plaid_item_id)
        .one()
    )
    request = AccountsBalanceGetRequest(access_token=plaid_item.access_token)
    try:
        response = client.accounts_balance_get(request)
        for acct in response["accounts"]:
            if acct["account_id"] == plaid_account.plaid_account_id:
                balance = acct.get("balances", {}).get("current") or acct.get(
                    "balances", {}
                ).get("available")
                if balance is not None:
                    record = PlaidAccountBalance(
                        plaid_account_id=plaid_account.id,
                        balance=balance,
                        timestamp=datetime.now(timezone.utc),
                    )
                    session.add(record)
                    session.commit()
                    return record
        return None
    except Exception as e:
        print(f"Error recording balance for Plaid account {plaid_account.id}: {e}")
        return None
