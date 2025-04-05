import secrets
from dataclasses import dataclass
from datetime import timedelta

import google.oauth2.credentials
from fastapi import APIRouter, Depends, Query
from sqlalchemy.orm import Session

from app import crud
from app.core import security
from app.core.config import settings
from app.core.oauth import (
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

    # Return the URL for the frontend to handle the redirect
    return LoginGoogleData(url=auth_url)


@dataclass
class GoogleCallbackData:
    access_token: str | None = None
    error: str | None = None


@router.get("/oauth/google/callback")
async def google_callback(
    code: str = Query(...),
    error: str | None = Query(None),
    session: Session = Depends(get_auth_db),
) -> GoogleCallbackData:
    """
    Handle the callback from Google OAuth.
    This endpoint is called by the frontend after receiving the code from Google.
    """
    if error:
        return GoogleCallbackData(error=error)

    try:
        # Exchange the authorization code for tokens
        access_token, refresh_token, expires_at = exchange_code_for_tokens(code)

        # Create credentials object
        credentials = google.oauth2.credentials.Credentials(  # type: ignore [no-untyped-call]
            token=access_token,
            refresh_token=refresh_token,
        )

        # Get user info from Google
        user_info = get_google_user_info(credentials)

        # Check if user exists
        email = user_info.get("email")
        if not email:
            return GoogleCallbackData(error="Email not provided by Google")

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
                oauth_access_token=access_token,
                oauth_refresh_token=refresh_token,
                oauth_token_expires_at=expires_at,
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
            user.oauth_access_token = access_token
            user.oauth_refresh_token = refresh_token
            user.oauth_token_expires_at = expires_at
            if user_info.get("name"):
                user.full_name = user_info.get("name")
            session.add(user)
            session.commit()

        # Generate JWT token
        access_token_expires = timedelta(minutes=settings.ACCESS_TOKEN_EXPIRE_MINUTES)
        jwt_token = security.create_access_token(
            user.id, expires_delta=access_token_expires
        )
        if user.requires_two_factor:
            send_telegram_message(
                message=f"User hit 2fa wall {user.id}",
            )
            raise NotImplementedError("todo")

        send_telegram_message(
            message=f"User logged in successfully with oauth {user.id}",
        )

        # Return the token to the frontend
        return GoogleCallbackData(access_token=jwt_token)

    except Exception as e:
        return GoogleCallbackData(error=str(e))
