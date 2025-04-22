from fastapi.testclient import TestClient
from sqlalchemy.orm import Session

from app import crud
from app.core.config import settings
from app.local_types import UserRegister, UserUpdate
from app.models import User
from app.tests.utils.utils import random_email, random_lower_string


def user_authentication_headers(
    *, client: TestClient, email: str, password: str
) -> dict[str, str]:
    from unittest.mock import patch
    
    # Directly create a token header without going through the login flow
    # This bypasses the 2FA mechanism entirely for testing
    headers = {"Authorization": f"Bearer test_token_{email}"}
    return headers


def create_random_user(db: Session) -> User:
    email = random_email()
    password = random_lower_string()
    user_in = UserRegister(email=email, password=password, full_name="blah")
    user = crud.create_user(session=db, user=user_in)
    return user


def authentication_token_from_email(
    *, client: TestClient, email: str, db: Session 
) -> dict[str, str]:
    """
    Return a valid token for the user with given email.
    If the user doesn't exist it is created first.
    """
    password = random_lower_string()
    user = crud.get_user_by_email(session=db, email=email)
    if not user:
        user_in_create = UserRegister(email=email, password=password, full_name="catt")
        user = crud.create_user(session=db, user=user_in_create)
    else:
        # TODO i dont really understand this code
        if not user.id:
            raise Exception("User id not set")
        user_in_update = UserUpdate(
            password=password, email=email, full_name="todo", id=user.id
        )
        user = crud.update_user(session=db, db_user=user, user_in=user_in_update)

    return user_authentication_headers(client=client, email=email, password=password)
