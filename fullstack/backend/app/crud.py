from dataclasses import dataclass
from typing import Literal
import json
from fastapi import HTTPException, Response

from app.core.security import (
    create_temp_token,
    get_password_hash,
    verify_password,
    verify_temp_token,
)
from app.db import Session
from app.local_types import UserRegister, UserUpdate
from app.models import User, UserSettings
from app.api.routes.two_factor import make_token_and_respond
from app.telegram_utils import send_telegram_message


def get_user_by_email(*, session: Session, email: str) -> User | None:
    return session.query(User).filter(User.email == email).one_or_none()


def get_user(*, session: Session, id: int) -> User | None:
    """Get a user by ID."""
    return session.query(User).filter(User.id == id).one_or_none()


def create_user(*, session: Session, user: UserRegister) -> User:
    existing_user = session.query(User).filter(User.email == user.email).one_or_none()
    if existing_user:
        raise ValueError("Email already registered")

    new_user = User(
        full_name=user.full_name,
        hashed_password=get_password_hash(user.password),
        email=user.email,
        settings=UserSettings(power_user_filters=True, has_budget=False),
    )
    session.add(new_user)
    session.commit()
    return new_user


def update_user(*, session: Session, db_user: User, user_in: UserUpdate) -> User:
    if user_in.password:
        db_user.hashed_password = get_password_hash(user_in.password)

    db_user.full_name = user_in.full_name
    db_user.email = user_in.email

    session.add(db_user)
    session.commit()
    return db_user


@dataclass(kw_only=True)
class InProgressUserLogin:
    requires_2fa: bool
    requires_2fa_setup: bool
    temp_token: str | None
    user: User

def determine_two_fa_status_from_user(user:User)-> InProgressUserLogin:
    # If 2FA is enabled for the user
    if user.totp_enabled:
            print("2FA is enabled for the user")
            temp_token = create_temp_token(user.id)
            return InProgressUserLogin(
                requires_2fa=True,
                requires_2fa_setup=False,
                temp_token=temp_token,
                user=user,
            )

    # User is required to set up 2FA but hasn't done so yet
    elif user.requires_two_factor and not user.totp_enabled:
        print("2FA is required but not set up for the user")
        temp_token = create_temp_token(user.id)
        return InProgressUserLogin(
            requires_2fa_setup=True,
            temp_token=temp_token,
            requires_2fa=False,
            user=user,
        )

    # the user has denied 2FA
    print("2FA is not required for the user")
    return InProgressUserLogin(
        user=user, requires_2fa=False, requires_2fa_setup=False, temp_token=None
    )

def handle_and_respond_to_in_progress_login(user: InProgressUserLogin, source: Literal['oauth', 'basic'])-> Response:
        if user.requires_2fa or user.requires_2fa_setup:
            send_telegram_message(
                message=f"User {user.user.email} needs 2FA verification",
            )
            # Return the 2FA challenge response with 200 status code
            response_data = {
                "access_token": None,
                "token_type": "bearer",
                "requires_2fa": user.requires_2fa,
                "requires_2fa_setup": user.requires_2fa_setup,
                "temp_token": user.temp_token,
            }
            return Response(
                content=json.dumps(response_data), media_type="application/json"
            )


        if not user.user.is_active:
            raise HTTPException(status_code=401, detail="Account is inactive")

        send_telegram_message(
            message=f"User logged in successfully {user.user.id} via {source}",
        )

        return make_token_and_respond(user_id=user.user.id)
   



def authenticate(
    *, session: Session, email: str, password: str
) -> InProgressUserLogin | None:
    db_user = get_user_by_email(session=session, email=email)

    if not db_user:
        return None
    if not verify_password(password, db_user.hashed_password):
        return None


    return determine_two_fa_status_from_user(db_user)



def get_user_by_temp_token(*, session: Session, token: str) -> User | None:
    """Get a user by a temporary token used during 2FA verification."""
    payload = verify_temp_token(token)
    if not payload:
        return None

    user_id = payload.get("sub")
    if not user_id:
        return None

    return get_user(session=session, id=int(user_id))


def enable_2fa(*, session: Session, user_id: int, secret: str) -> User:
    """Store the TOTP secret for a user."""
    user = get_user(session=session, id=user_id)
    if not user:
        raise ValueError("User not found")

    user.totp_secret = secret
    session.commit()
    return user


def activate_2fa(*, session: Session, user_id: int) -> User:
    """Activate 2FA for a user after they've verified their setup."""
    user = get_user(session=session, id=user_id)
    if not user:
        raise ValueError("User not found")

    if not user.totp_secret:
        raise ValueError("TOTP secret not set")

    user.totp_enabled = True
    session.commit()
    return user


def disable_2fa(*, session: Session, user_id: int) -> User:
    """Disable 2FA for a user."""
    user = get_user(session=session, id=user_id)
    if not user:
        raise ValueError("User not found")

    user.totp_enabled = False
    user.totp_secret = None
    session.commit()
    return user
