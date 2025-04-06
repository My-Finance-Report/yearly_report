from dataclasses import dataclass
from app.core.security import get_password_hash, verify_password, create_temp_token, verify_temp_token
from app.db import Session
from app.local_types import UserRegister, UserUpdate
from app.models import User, UserSettings
from app.telegram_utils import send_telegram_message
from fastapi import HTTPException


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

def authenticate(*, session: Session, email: str, password: str) ->   InProgressUserLogin | None:
    db_user = get_user_by_email(session=session, email=email)

    if not db_user:
        return None
    if not verify_password(password, db_user.hashed_password):
        return None
        
    
    # If 2FA is enabled for the user
    if db_user.totp_enabled:
        print("2FA is enabled for the user")
        temp_token = create_temp_token(db_user.id)
        return InProgressUserLogin(
            requires_2fa=True,
            requires_2fa_setup=False,
            temp_token=temp_token,
            user=db_user
        )

    # User is required to set up 2FA but hasn't done so yet
    elif db_user.requires_two_factor and not db_user.totp_enabled:
        print("2FA is required but not set up for the user")
        temp_token = create_temp_token(db_user.id)
        return InProgressUserLogin(
            requires_2fa_setup=True,
            temp_token=temp_token,
            requires_2fa=False,
            user=db_user
        )
    
    # the user has denied 2FA
    print("2FA is not required for the user")
    return InProgressUserLogin(
        user=db_user,
        requires_2fa=False,
        requires_2fa_setup=False,
        temp_token=None
    )


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
