from collections.abc import Generator

import pytest
from fastapi.testclient import TestClient
from sqlalchemy.orm import Session

from app.core.config import settings
from app.core.db import engine, init_db
from app.db import get_db_for_user
from app.main import app
from app.models.user import User
from app.tests.utils.user import authentication_token_from_email
from app.tests.utils.utils import (
    get_superuser_token_headers,
    TestKit,
    random_lower_string,
)


@pytest.fixture(scope="session", autouse=True)
def db() -> Generator[Session, None, None]:
    with Session(engine) as session:
        init_db(session)
        yield session


def _db() -> Generator[Session, None, None]:
    with Session(engine) as session:
        init_db(session)
        yield session


@pytest.fixture(scope="session", autouse=True)
def test_kit() -> TestKit:
    # Create test user
    db_ = next(_db())
    user = User(
        email=f"{random_lower_string()}@example.com",
        hashed_password="test_password",
        full_name="Test User",
        is_active=True,
    )
    db_.add(user)
    db_.commit()
    session = next(get_db_for_user(user.id))
    return TestKit(session=session, user=user)


@pytest.fixture(scope="module")
def client() -> Generator[TestClient, None, None]:
    with TestClient(app) as c:
        yield c


@pytest.fixture(scope="module")
def superuser_token_headers(client: TestClient) -> dict[str, str]:
    return get_superuser_token_headers(client)


@pytest.fixture(scope="module")
def normal_user_token_headers(client: TestClient, db: Session) -> dict[str, str]:
    return authentication_token_from_email(
        client=client, email=settings.EMAIL_TEST_USER, db=db
    )
