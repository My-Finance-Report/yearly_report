import secrets
from dataclasses import dataclass
from typing import Any

import google.oauth2.credentials
from fastapi import APIRouter, Depends, Query, Response
from sqlalchemy.orm import Session

from app import crud
from app.core.oauth import (
    GoogleTokenResponse,
    exchange_code_for_tokens,
    get_google_authorization_url,
    get_google_user_info,
)
from app.db import get_auth_db
from app.models import User, UserSettings
from app.telegram_utils import send_telegram_message

router = APIRouter(tags=["oauth"])


@dataclass
class LoginGoogleData:
    url: str


@router.get("/oauth/google/login")
async def login_google() -> LoginGoogleData:
    """
    Initiate Google OAuth login flow.
    Returns the authorization URL that the frontend should redirect to.
    """
    # Generate a state token to prevent CSRF
    state = secrets.token_urlsafe(32)

    # Store state in session or DB for verification later
    # For simplicity, we'll use a query param in the redirect URI

    # Get the Google authorization URL
    auth_url = get_google_authorization_url(state)

    # TODO set env variables in prod so this doesnt trip
    # Return the URL for the frontend to handle the redirect
    # if get_env() == "local" and get_env() != "production":
    #    mock_auth_url = "http://127.0.0.1:5173/oauth-callback-local"
    #    return LoginGoogleData(url=mock_auth_url)

    return LoginGoogleData(url=auth_url)


def get_and_update_user(
    session: Session, user_info: Any, token_response: GoogleTokenResponse
) -> User:
    # Check if user exists
    email = user_info.get("email")
    if not email:
        raise ValueError("Email not provided by google")

    user = crud.get_user_by_email(session=session, email=email)

    if not user:
        # Create a new user
        user = User(
            email=email,
            full_name=user_info.get("name"),
            is_active=True,
            is_superuser=False,
            hashed_password=None,
            oauth_provider="google",
            oauth_id=user_info.get("id"),
            oauth_access_token=token_response.access_token,
            oauth_refresh_token=token_response.refresh_token,
            oauth_token_expires_at=token_response.expires_at,
            settings=UserSettings(has_budget=False, power_user_filters=False),
        )
        session.add(user)
        session.commit()

        send_telegram_message(
            message=f"New user created via oauth: {user.id}",
        )

    else:
        # Update existing user with new tokens
        user.oauth_provider = "google"
        user.oauth_id = user_info.get("id")
        user.oauth_access_token = token_response.access_token
        user.oauth_refresh_token = token_response.refresh_token
        user.oauth_token_expires_at = token_response.expires_at
        if user_info.get("name"):
            user.full_name = user_info.get("name")
        session.add(user)
        session.commit()

    return user


@router.get("/oauth/google/callback")
async def google_callback(
    code: str = Query(...),
    error: str | None = Query(None),
    session: Session = Depends(get_auth_db),
) -> Response:
    """
    Handle the callback from Google OAuth.
    This endpoint is called by the frontend after receiving the code from Google.
    """
    if error:
        return Response(content=error, status_code=400)

    try:
        token_response = exchange_code_for_tokens(code)

        credentials = google.oauth2.credentials.Credentials(  # type: ignore [no-untyped-call]
            token=token_response.access_token,
            refresh_token=token_response.refresh_token,
        )

        user_info = get_google_user_info(credentials)

        user = get_and_update_user(
            session=session, user_info=user_info, token_response=token_response
        )

        return after_google_portion_of_auth(user)

    except Exception as e:
        return Response(content=str(e), status_code=500)


def after_google_portion_of_auth(user: User) -> Response:
    """
    shared helper to make sure the local mock and the prod instance both work the same
    """

    in_progress_login_user = crud.determine_two_fa_status_from_user(user)
    return crud.handle_and_respond_to_in_progress_login(in_progress_login_user, "oauth")
