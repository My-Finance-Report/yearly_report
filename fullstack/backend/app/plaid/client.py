import os
from typing import Optional

import plaid
from plaid.api import plaid_api
from plaid.model.link_token_create_request import LinkTokenCreateRequest
from plaid.model.link_token_create_request_user import LinkTokenCreateRequestUser
from plaid.model.item_public_token_exchange_request import ItemPublicTokenExchangeRequest
from plaid.model.products import Products
from plaid.model.country_code import CountryCode

def get_plaid_client():
    """Initialize and return a Plaid client."""
    configuration = plaid.Configuration(
        host=plaid.Environment.Sandbox,  # Change to Development or Production as needed
        api_key={
            'clientId': os.getenv('PLAID_CLIENT_ID', ''),
            'secret': os.getenv('PLAID_SECRET', ''),
        }
    )
    api_client = plaid.ApiClient(configuration)
    return plaid_api.PlaidApi(api_client)

def create_link_token(user_id: str, client_name: str = "My Finance App") -> str:
    """Create a link token for a user."""
    client = get_plaid_client()
    
    # Create a link token for the given user
    request = LinkTokenCreateRequest(
        user=LinkTokenCreateRequestUser(
            client_user_id=str(user_id)
        ),
        client_name=client_name,
        products=[Products("transactions")],
        country_codes=[CountryCode("US")],
        language="en",
    )
    
    response = client.link_token_create(request)
    return response['link_token']

def exchange_public_token(public_token: str) -> dict:
    """Exchange a public token for an access token and item ID."""
    client = get_plaid_client()
    
    request = ItemPublicTokenExchangeRequest(
        public_token=public_token
    )
    
    response = client.item_public_token_exchange(request)
    return {
        'access_token': response['access_token'],
        'item_id': response['item_id']
    }
