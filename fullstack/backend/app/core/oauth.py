from dataclasses import dataclass
from datetime import datetime, timedelta, timezone
from typing import Any

from google.oauth2.credentials import Credentials
from google_auth_oauthlib.flow import Flow

from app.core.config import settings

# Google OAuth configuration
GOOGLE_CLIENT_CONFIG = {
    "web": {
        "client_id": settings.GOOGLE_CLIENT_ID,
        "client_secret": settings.GOOGLE_CLIENT_SECRET,
        "auth_uri": "https://accounts.google.com/o/oauth2/auth",
        "token_uri": "https://oauth2.googleapis.com/token",
        "redirect_uris": [f"{settings.SERVER_HOST}/oauth-callback"],
        "javascript_origins": [settings.SERVER_HOST],
    }
}

GOOGLE_SCOPES = [
    "https://www.googleapis.com/auth/userinfo.email",
    "https://www.googleapis.com/auth/userinfo.profile",
    "openid",
]


def create_google_auth_flow(redirect_uri: str | None = None) -> Flow:
    """Create a Google OAuth flow instance."""
    flow = Flow.from_client_config(
        client_config=GOOGLE_CLIENT_CONFIG,
        scopes=GOOGLE_SCOPES,
    )

    if redirect_uri:
        flow.redirect_uri = redirect_uri
    else:
        flow.redirect_uri = GOOGLE_CLIENT_CONFIG["web"]["redirect_uris"][0]

    return flow


def get_google_authorization_url(state: str) -> str:
    """Generate the Google authorization URL."""
    flow = create_google_auth_flow()
    val: tuple[str, str] = flow.authorization_url(
        access_type="offline",
        include_granted_scopes="true",
        state=state,
        prompt="consent",
    )
    return val[0]


@dataclass
class GoogleTokenResponse:
    access_token: str
    refresh_token: str
    expires_at: datetime

def exchange_code_for_tokens(code: str) -> GoogleTokenResponse:
    """Exchange authorization code for access and refresh tokens."""
    # Get the redirect URI from the request
    redirect_uri = f"{settings.SERVER_HOST}/oauth-callback"

    flow = create_google_auth_flow(redirect_uri)
    flow.fetch_token(code=code)

    credentials = flow.credentials
    expires_at = datetime.now(timezone.utc) + timedelta(
        seconds=credentials.expiry.timestamp() - datetime.now().timestamp()
    )

    return GoogleTokenResponse(
        access_token=credentials.token,
        refresh_token=credentials.refresh_token,
        expires_at=expires_at,
    )


def get_google_user_info(credentials: Credentials) -> Any:
    """Fetch user information from Google API."""
    import requests

    headers = {"Authorization": f"Bearer {credentials.token}"}

    response = requests.get(
        "https://www.googleapis.com/oauth2/v1/userinfo", headers=headers
    )

    if response.status_code != 200:
        raise Exception(f"Failed to get user info: {response.text}")

    return response.json()
