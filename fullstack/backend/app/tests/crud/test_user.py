from fastapi.encoders import jsonable_encoder
from sqlmodel import Session

from app import crud
from app.core.security import verify_password
from app.local_types import UserRegister, UserUpdate
from app.models import User
from app.tests.utils.utils import random_email, random_lower_string


def test_create_user(db: Session) -> None:
    email = random_email()
    password = random_lower_string()
    user_in = UserRegister(
        email=email,
        password=password,
        full_name="catt",
    )
    user = crud.create_user(session=db, user=user_in)
    assert user.email == email
    assert hasattr(user, "hashed_password")


def test_authenticate_user(db: Session) -> None:
    email = random_email()
    password = random_lower_string()
    user_in = UserRegister(
        email=email,
        password=password,
        full_name="catt",
    )
    user = crud.create_user(session=db, user=user_in)
    authenticated_user = crud.authenticate(
        session=db,
        email=email,
        password=password,
    )
    assert authenticated_user
    assert authenticated_user.user.email == email
    assert user.email == authenticated_user.user.email


def test_not_authenticate_user(db: Session) -> None:
    email = random_email()
    password = random_lower_string()
    user = crud.authenticate(
        session=db,
        email=email,
        password=password,
    )
    assert user is None


def test_check_if_user_is_active(db: Session) -> None:
    email = random_email()
    password = random_lower_string()
    user_in = UserRegister(email=email, password=password, full_name="catt")
    user = crud.create_user(session=db, user=user_in)
    assert user.is_active is True


def test_check_if_user_is_active_inactive(db: Session) -> None:
    email = random_email()
    password = random_lower_string()
    user_in = UserRegister(
        email=email, password=password, full_name="catt", is_active=False
    )
    user = crud.create_user(session=db, user=user_in)
    assert user.is_active


def test_check_if_user_is_superuser(db: Session) -> None:
    email = random_email()
    password = random_lower_string()
    # seem unintuative, but make sure that someone _cannot_ make themselves a superuser
    # basically we dont want any code to be able to set is_superuser to true
    user_in = UserRegister(
        email=email, password=password, full_name="catt", is_superuser=True
    )
    user = crud.create_user(session=db, user=user_in)
    assert user.is_superuser is False


def test_check_if_user_is_superuser_normal_user(db: Session) -> None:
    username = random_email()
    password = random_lower_string()
    user_in = UserRegister(email=username, password=password, full_name="catt")
    user = crud.create_user(session=db, user=user_in)
    assert user.is_superuser is False


def test_get_user(db: Session) -> None:
    password = random_lower_string()
    username = random_email()
    user_in = UserRegister(
        email=username, password=password, full_name="catt", is_superuser=True
    )
    user = crud.create_user(session=db, user=user_in)
    user_2 = db.get(User, user.id)
    assert user_2
    assert user.email == user_2.email
    assert jsonable_encoder(user) == jsonable_encoder(user_2)


def test_update_user(db: Session) -> None:
    password = random_lower_string()
    email = random_email()
    user_in = UserRegister(
        email=email, password=password, full_name="catt", is_superuser=True
    )
    user = crud.create_user(session=db, user=user_in)
    new_password = random_lower_string()
    user_in_update = UserUpdate(
        password=new_password,
        is_superuser=True,
        email=email,
        full_name="catt",
        id=user.id,
    )
    if user.id is not None:
        crud.update_user(session=db, db_user=user, user_in=user_in_update)
    user_2 = db.get(User, user.id)
    assert user_2
    assert user.email == user_2.email
    assert verify_password(new_password, user_2.hashed_password)
