import os

import plaid
from plaid.api import plaid_api
from plaid.model.country_code import CountryCode
from plaid.model.item_public_token_exchange_request import (
    ItemPublicTokenExchangeRequest,
)
from plaid.model.link_token_create_request import LinkTokenCreateRequest
from plaid.model.link_token_create_request_user import LinkTokenCreateRequestUser
from plaid.model.products import Products


def get_plaid_client() -> plaid_api.PlaidApi:
    """Initialize and return a Plaid client."""
    client_id = os.environ["PLAID_CLIENT_ID"]
    secret = os.environ["PLAID_SECRET"]
    environment = os.environ["PLAID_ENV"]

    configuration = plaid.Configuration(
        host=plaid.Environment.Sandbox if environment == "sandbox" else plaid.Environment.Production,
        api_key={
            "clientId": client_id,
            "secret": secret,
        },
    )
    api_client = plaid.ApiClient(configuration)
    return plaid_api.PlaidApi(api_client)


def create_link_token(user_id: str, client_name: str = "My Finance App") -> str:
    """Create a link token for a user."""
    client = get_plaid_client()

    # Create a link token for the given user
    request = LinkTokenCreateRequest(
        user=LinkTokenCreateRequestUser(client_user_id=str(user_id)),
        client_name=client_name,
        products=[Products("transactions")],
        country_codes=[CountryCode("US")],
        language="en",
    )

    response = client.link_token_create(request)
    val = response["link_token"]
    assert isinstance(val, str)
    return val


def exchange_public_token(public_token: str) -> dict[str, str]:
    """Exchange a public token for an access token and item ID."""
    client = get_plaid_client()

    request = ItemPublicTokenExchangeRequest(public_token=public_token)

    response = client.item_public_token_exchange(request)
    return {"access_token": response["access_token"], "item_id": response["item_id"]}
