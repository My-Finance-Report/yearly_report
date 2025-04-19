from plaid.api.plaid_api import AccountsBalanceGetRequest
from app.plaid.client import get_plaid_client
from typing import Any


def safe_parse_float(val: Any) -> float | None:
    try:
        return float(val)
    except:
        return None


def fetch_account_balance(access_token: str, plaid_account_id: str) -> float | None:
    """Fetch the current balance for a specific Plaid account."""
    client = get_plaid_client()
    request = AccountsBalanceGetRequest(access_token=access_token)
    try:
        response = client.accounts_balance_get(request)
        for acct in response["accounts"]:
            if acct["account_id"] == plaid_account_id:
                return safe_parse_float(
                    acct.get("balances", {}).get("current")
                    or acct.get("balances", {}).get("available")
                )
        return None
    except Exception as e:
        print(f"Error fetching balance for account {plaid_account_id}: {e}")
        return None
