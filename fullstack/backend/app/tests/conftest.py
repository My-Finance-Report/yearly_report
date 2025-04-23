from collections.abc import Generator

import pytest
from fastapi.testclient import TestClient
from sqlalchemy import text
from sqlalchemy.orm import Session

from app.core.config import settings
from app.core.db import engine, init_db
from app.main import app
from app.models import User
from app.tests.utils.user import authentication_token_from_email
from app.tests.utils.utils import get_superuser_token_headers


@pytest.fixture(scope="function")
def db() -> Generator[Session, None, None]:
    # Connect to the database and begin a transaction
    connection = engine.connect()
    transaction = connection.begin()
    
    # Create a session bound to the connection
    session = Session(bind=connection)
    
    try:
        yield session
    finally:
        # Roll back the transaction after the test completes
        session.close()
        transaction.rollback()
        connection.close()


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
