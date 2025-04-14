import uuid

from fastapi import APIRouter, Depends, HTTPException
from plaid.api.plaid_api import AccountsGetRequest
from sqlalchemy.orm import Session

from app.async_pipelines.uploaded_file_pipeline.configuration_creator import (
    add_default_categories,
)
from app.db import get_current_user, get_db
from app.models import (
    PlaidAccount,
    PlaidItem,
    ProcessingState,
    SourceKind,
    TransactionSource,
    User,
)
from app.plaid.client import create_link_token, exchange_public_token, get_plaid_client
from app.plaid.models import (
    PlaidAccountResponse,
    PlaidExchangeTokenRequest,
    PlaidLinkTokenResponse,
)
from app.telegram_utils import send_telegram_message
from app.worker.status import update_worker_status

router = APIRouter(prefix="/plaid", tags=["plaid"])


@router.post("/create_link_token", response_model=PlaidLinkTokenResponse)
def get_link_token(
    user: User = Depends(get_current_user),
) -> PlaidLinkTokenResponse:
    """Create a link token for Plaid Link."""
    send_telegram_message(
        message=f"User requested link token {user.id}",
    )
    try:
        link_token = create_link_token(str(user.id))
        return PlaidLinkTokenResponse(link_token=link_token)
    except Exception:
        raise HTTPException(status_code=500, detail="Error creating link token:")


def find_unique_name(
    name: str, session: Session, user: User, prev_existing_names: list[str]
) -> tuple[str, list[str]]:
    existing_names = (
        session.query(TransactionSource.name)
        .filter(
            TransactionSource.user_id == user.id,
            TransactionSource.name.contains(name),
        )
        .all()
    )

    existing_names_strings: list[str] = [name[0] for name in existing_names]
    existing_names_strings.extend(prev_existing_names)

    if not existing_names:
        return name, []

    for i in range(2, 100):
        new_name = f"{name} ({i})"
        if new_name not in existing_names_strings:
            existing_names_strings.append(new_name)
            return (new_name, existing_names_strings)

    raise HTTPException(status_code=500, detail="Failed to generate unique name")


@router.post("/exchange_token", response_model=list[PlaidAccountResponse])
async def exchange_token(
    request: PlaidExchangeTokenRequest,
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> list[PlaidAccountResponse]:
    """Exchange a public token for an access token and get accounts."""
    try:
        # Exchange the public token for an access token
        exchange_response = exchange_public_token(request.public_token)
        access_token = exchange_response["access_token"]
        item_id = exchange_response["item_id"]

        plaid_item = PlaidItem(
            user_id=user.id,
            plaid_item_id=item_id,
            access_token=access_token,
        )
        session.add(plaid_item)
        session.commit()

    except Exception as e:
        print(e)
        raise HTTPException(status_code=500, detail="Error exchanging token")

    existing_names: set[str] = set()
    try:
        client = get_plaid_client()
        request = AccountsGetRequest(access_token=access_token)
        accounts_response = client.accounts_get(request)
        created_accounts = []
        for account in accounts_response["accounts"]:
            plaid_account = PlaidAccount(
                user_id=user.id,
                plaid_item_id=plaid_item.id,
                plaid_account_id=account["account_id"],
                name=account["name"],
                mask=account.get("mask"),
                type=str(account["type"]),
                subtype=str(account.get("subtype")),
            )
            session.add(plaid_account)
            session.flush()

            name, new_existing_names = find_unique_name(
                f"{account['name']}", session, user, list(existing_names)
            )

            for name in new_existing_names:
                existing_names.add(name)

            transaction_source = TransactionSource(
                user_id=user.id,
                name=name,
                plaid_account_id=plaid_account.id,
                source_kind=get_source_kind_from_account_type(account["type"]),
            )
            session.add(transaction_source)

            session.commit()
            session.refresh(transaction_source)

            add_default_categories(session, user, transaction_source)

            created_accounts.append(
                PlaidAccountResponse(
                    id=plaid_account.id,
                    plaid_account_id=plaid_account.plaid_account_id,
                    name=plaid_account.name,
                    mask=plaid_account.mask,
                    type=plaid_account.type,
                    subtype=plaid_account.subtype,
                    created_at=plaid_account.created_at,
                )
            )

        session.commit()
        send_telegram_message(
            message=f"Successfully added Plaid accounts for user {user.id}"
        )
        batch_id = str(uuid.uuid4())
        update_worker_status(
            session=session,
            user=user,
            status=ProcessingState.waiting,
            additional_info="Waiting for process to begin fetching transactions",
            batch_id=batch_id,
        )
        return created_accounts
    except Exception as e:
        send_telegram_message(message=f"Error getting accounts: {str(e)}")
        raise HTTPException(status_code=500, detail="Error getting accounts")


@router.get("/accounts", response_model=list[PlaidAccountResponse])
def get_plaid_accounts(
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> list[PlaidAccountResponse]:
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


def get_source_kind_from_account_type(account_type: str) -> SourceKind:
    """Map Plaid account types to SourceKind."""
    INVESTMENT_ACCOUNT_TYPES = [
        "brokerage",
        "401a",
        "401k",
        "403b",
        "457b",
        "529",
        "ira",
        "roth ira",
        "roth 401k",
        "sep ira",
        "simple ira",
        "esa",
        "isa",
        "lira",
        "variable annuity",
    ]
    CREDIT_ACCOUNT_TYPES = [
        "credit card",
        "line of credit",
        "mortgage",
        "home equity",
        "auto",
        "student",
    ]

    if any(credit_type in account_type for credit_type in CREDIT_ACCOUNT_TYPES):
        return SourceKind.card
    elif any(
        investment_type in account_type for investment_type in INVESTMENT_ACCOUNT_TYPES
    ):
        return SourceKind.investment
    else:
        return SourceKind.account
