from typing import List
from fastapi import APIRouter, Depends, HTTPException
from sqlalchemy.orm import Session

from app.db import get_current_user, get_db
from app.models import (
    PlaidItem,
    PlaidAccount,
    TransactionSource,
    User,
)
from app.plaid.client import create_link_token, exchange_public_token, get_plaid_client
from app.plaid.models import (
    PlaidLinkTokenRequest,
    PlaidLinkTokenResponse,
    PlaidExchangeTokenRequest,
    PlaidAccountResponse,
)

router = APIRouter(prefix="/plaid", tags=["plaid"])


@router.post("/create_link_token", response_model=PlaidLinkTokenResponse)
def get_link_token(
    request: PlaidLinkTokenRequest,
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> PlaidLinkTokenResponse:
    """Create a link token for Plaid Link."""
    try:
        link_token = create_link_token(str(user.id))
        return PlaidLinkTokenResponse(link_token=link_token)
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Error creating link token: {str(e)}")


@router.post("/exchange_token", response_model=List[PlaidAccountResponse])
async def exchange_token(
    request: PlaidExchangeTokenRequest,
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> List[PlaidAccountResponse]:
    """Exchange a public token for an access token and get accounts."""
    try:
        # Exchange the public token for an access token
        exchange_response = exchange_public_token(request.public_token)
        access_token = exchange_response['access_token']
        item_id = exchange_response['item_id']
        
        # Create a new Plaid item
        plaid_item = PlaidItem(
            user_id=user.id,
            plaid_item_id=item_id,
            access_token=access_token,
        )
        session.add(plaid_item)
        session.flush()  # Flush to get the ID
        
        # Get accounts from Plaid
        client = get_plaid_client()
        accounts_response = client.accounts_get(access_token)
        
        # Create Plaid accounts and transaction sources
        created_accounts = []
        for account in accounts_response['accounts']:
            # Create Plaid account
            plaid_account = PlaidAccount(
                user_id=user.id,
                plaid_item_id=plaid_item.id,
                plaid_account_id=account['account_id'],
                name=account['name'],
                mask=account.get('mask'),
                type=account['type'],
                subtype=account.get('subtype'),
            )
            session.add(plaid_account)
            session.flush()  # Flush to get the ID
            
            transaction_source = TransactionSource(
                user_id=user.id,
                name=f"{account['name']} (Plaid)",
                plaid_account_id=plaid_account.id,
                source_kind=get_source_kind_from_account_type(account['type']),
            )
            session.add(transaction_source)
            
            created_accounts.append(PlaidAccountResponse(
                id=plaid_account.id,
                plaid_account_id=plaid_account.plaid_account_id,
                name=plaid_account.name,
                mask=plaid_account.mask,
                type=plaid_account.type,
                subtype=plaid_account.subtype,
                created_at=plaid_account.created_at,
            ))
        
        session.commit()
        return created_accounts
    
    except Exception as e:
        session.rollback()
        raise HTTPException(status_code=500, detail=f"Error exchanging token: {str(e)}")


@router.get("/accounts", response_model=List[PlaidAccountResponse])
def get_plaid_accounts(
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> List[PlaidAccountResponse]:
    accounts = session.query(PlaidAccount).filter(PlaidAccount.user_id == user.id).all()
    return [
        PlaidAccountResponse(
            id=account.id,
            plaid_account_id=account.plaid_account_id,
            name=account.name,
            mask=account.mask,
            type=account.type,
            subtype=account.subtype,
            created_at=account.created_at,
        )
        for account in accounts
    ]


def get_source_kind_from_account_type(account_type: str) -> str:
    """Map Plaid account types to SourceKind."""
    if account_type == "credit":
        return "card"
    elif account_type == "investment":
        return "investment"
    else:
        return "account"
