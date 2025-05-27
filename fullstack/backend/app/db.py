from collections.abc import Generator
from typing import Any

import jwt
import sqlalchemy.orm as orm
from dotenv import load_dotenv
from fastapi import Depends, HTTPException, Request, status
from fastapi.security import OAuth2PasswordBearer
from jwt.exceptions import InvalidTokenError
from pydantic import ValidationError
from sqlalchemy import create_engine, text

from app.core import security
from app.core.config import settings
from app.get_db_string import get_app_user_database_url
from app.local_types import TokenPayload
from app.models.user import User

load_dotenv()

DATABASE_URL = get_app_user_database_url()

engine = create_engine(
    DATABASE_URL, echo=False, pool_use_lifo=True, pool_size=20, max_overflow=40
)

session_maker = orm.sessionmaker(autoflush=True, bind=engine)
type Session = orm.Session


reusable_oauth2 = OAuth2PasswordBearer(
    tokenUrl=f"{settings.API_V1_STR}/login/access-token"
)


def get_token_from_cookie(request: Request) -> str:
    token = request.cookies.get("access_token")
    if not token:
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="Missing access token cookie",
        )
    return token


def get_token_from_cookie_optional(request: Request) -> str | None:
    return request.cookies.get("access_token")


def get_auth_db() -> Generator[Session, Any, None]:
    """Returns a database session that does NOT enforce RLS (used for authentication)."""

    # can't set user for rls because there is no user
    db = session_maker()

    try:
        yield db
    finally:
        db.close()


def get_current_user(
    token: str | bytes = Depends(get_token_from_cookie),
    session: Session = Depends(get_auth_db),
) -> User:
    try:
        payload = jwt.decode(
            token, settings.SECRET_KEY, algorithms=[security.ALGORITHM]
        )

        token_data = TokenPayload(**payload)

    except (InvalidTokenError, ValidationError) as e:
        print("Invalid token", e)
        raise HTTPException(
            status_code=status.HTTP_403_FORBIDDEN,
            detail="Could not validate credentials",
        )

    # Query the user by ID
    user = session.query(User).filter(User.id == token_data.sub).first()

    if not user:
        raise HTTPException(status_code=404, detail="User not found")
    if not user.is_active:
        raise HTTPException(status_code=400, detail="Inactive user")

    return user


def get_current_user_optional(
    token: str | bytes = Depends(get_token_from_cookie_optional),
    session: Session = Depends(get_auth_db),
) -> User | None:
    try:
        payload = jwt.decode(
            token, settings.SECRET_KEY, algorithms=[security.ALGORITHM]
        )

        token_data = TokenPayload(**payload)

    except (InvalidTokenError, ValidationError):
        return None

    # Query the user by ID
    user = session.query(User).filter(User.id == token_data.sub).first()

    if not user:
        return None
    if not user.is_active:
        return None

    return user


def get_current_user_from_temp_token(
    token: str | bytes = Depends(reusable_oauth2),
    session: Session = Depends(get_auth_db),
) -> User:
    """
    Similar to get_current_user but designed to work with temporary tokens
    issued during the 2FA setup process.
    """
    try:
        payload = jwt.decode(
            token, settings.SECRET_KEY, algorithms=[security.ALGORITHM]
        )
        token_data = TokenPayload(**payload)

    except (InvalidTokenError, ValidationError):
        raise HTTPException(
            status_code=status.HTTP_403_FORBIDDEN,
            detail="Could not validate credentials",
        )

    # Query the user by ID
    user = session.query(User).filter(User.id == token_data.sub).first()

    if not user:
        raise HTTPException(status_code=404, detail="User not found")
    if not user.is_active:
        raise HTTPException(status_code=400, detail="Inactive user")

    return user


def get_current_active_superuser(
    current_user: User = Depends(get_current_user),
) -> User:
    if not current_user.is_superuser:
        raise HTTPException(
            status_code=403, detail="The user doesn't have enough privileges"
        )
    return current_user


def get_db(user: User = Depends(get_current_user)) -> Generator[Session, Any, None]:
    """Returns a database session and sets the user ID for RLS policies."""
    db = session_maker()

    db.execute(text(f"SET app.current_user_id = {user.id}"))
    db.commit()

    if db.execute(
        text("SELECT current_setting('app.current_user_id')")
    ).scalar() != str(user.id):
        raise NotImplementedError("idk why this isnt working")

    try:
        yield db
    finally:
        db.close()


def get_db_for_user(user_id: int) -> Generator[Session, Any, None]:
    """Returns a database session and sets the user ID for RLS policies."""
    db = session_maker()

    db.execute(text(f"SET app.current_user_id = {user_id}"))
    db.commit()

    if db.execute(
        text("SELECT current_setting('app.current_user_id')")
    ).scalar() != str(user_id):
        raise NotImplementedError("idk why this isnt working")

    try:
        yield db
    finally:
        db.close()
