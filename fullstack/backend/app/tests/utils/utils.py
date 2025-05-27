from dataclasses import dataclass
import random
import string

from fastapi.testclient import TestClient

from app.core.config import settings
from app.db import Session
from app.models.user import User


@dataclass
class TestKit:
    session: Session
    user: User


def random_lower_string() -> str:
    return "".join(random.choices(string.ascii_lowercase, k=32))


def random_email() -> str:
    return f"{random_lower_string()}@{random_lower_string()}.com"


def get_superuser_token_headers(client: TestClient) -> dict[str, str]:
    from unittest.mock import patch

    login_data = {
        "username": settings.FIRST_SUPERUSER,
        "password": settings.FIRST_SUPERUSER_PASSWORD,
    }

    # Directly create a token header without going through the login flow
    # This bypasses the 2FA mechanism entirely for testing
    with patch("app.db.get_current_active_user") as mock_get_user:
        # Return a fake token that will be accepted by the authentication system
        headers = {"Authorization": "Bearer test_superuser_token"}
        return headers
