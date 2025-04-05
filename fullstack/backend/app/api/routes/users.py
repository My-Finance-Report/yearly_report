import uuid
from typing import Any

from fastapi import APIRouter, Depends, HTTPException
from sqlalchemy import func

from app import crud
from app.core.config import settings
from app.core.security import get_password_hash, verify_password
from app.db import (
    Session,
    get_auth_db,
    get_current_active_superuser,
    get_current_user,
    get_db,
)
from app.local_types import (
    Message,
    UserNewPassword,
    UserOut,
    UserRegister,
    UsersPublic,
    UserUpdate,
    UserUpdateMe,
)
from app.models import (
    User,
)
from app.telegram_utils import send_telegram_message
from app.utils import generate_new_account_email, send_email

router = APIRouter(prefix="/users", tags=["users"])


@router.get(
    "/",
    dependencies=[Depends(get_current_active_superuser)],
    response_model=UsersPublic,
)
def read_users(
    session: Session = Depends(get_db), skip: int = 0, limit: int = 100
) -> UsersPublic:
    """
    Retrieve users. NOTE: this doesnt work due to RLS policy
    """

    count = session.query(func.count()).select_from(User).scalar()

    users = session.query(User).offset(skip).limit(limit).all()

    return UsersPublic(
        data=[
            UserOut(
                full_name=user.full_name or "no name user",
                email=user.email,
                id=user.id,
                is_superuser=user.is_superuser,
                settings=user.settings,
            )
            for user in users
        ],
        count=count,
    )


@router.post(
    "/", dependencies=[Depends(get_current_active_superuser)], response_model=UserOut
)
def create_user(*, session: Session = Depends(get_db), user_in: UserRegister) -> Any:
    """
    Create a new user.
    """
    user = crud.get_user_by_email(session=session, email=user_in.email)
    if user:
        raise HTTPException(
            status_code=400,
            detail="The user with this email already exists in the system.",
        )

    user = crud.create_user(session=session, user=user_in)
    if settings.emails_enabled and user_in.email:
        email_data = generate_new_account_email(
            email_to=user_in.email, username=user_in.email, password=user_in.password
        )
        send_email(
            email_to=user_in.email,
            subject=email_data.subject,
            html_content=email_data.html_content,
        )
    return user


@router.patch("/me", response_model=UserOut)
def update_user_me(
    *,
    session: Session = Depends(get_db),
    user_in: UserUpdateMe,
    current_user: User = Depends(get_current_user),
) -> UserOut:
    """
    Update own user.
    """

    db_user = session.query(User).filter(User.id == current_user.id).one()

    if user_in.email and db_user.email != user_in.email:
        existing_user = crud.get_user_by_email(session=session, email=user_in.email)
        if existing_user and existing_user.id != current_user.id:
            raise HTTPException(
                status_code=409, detail="User with this email already exists"
            )
        db_user.email = user_in.email

    if user_in.settings:
        db_user.settings = user_in.settings

    session.commit()
    session.refresh(db_user)
    return UserOut(
        full_name=db_user.full_name or "no name user",
        email=db_user.email,
        id=db_user.id,
        is_superuser=db_user.is_superuser,
        settings=db_user.settings,
    )


@router.patch("/me/password", response_model=Message)
def update_password_me(
    *,
    session: Session = Depends(get_db),
    body: UserNewPassword,
    current_user: User = Depends(get_current_user),
) -> Any:
    """
    Update own password.
    """
    if not verify_password(body.old_password, current_user.hashed_password):
        raise HTTPException(status_code=400, detail="Incorrect password")
    if body.old_password == body.new_password:
        raise HTTPException(
            status_code=400, detail="New password cannot be the same as the current one"
        )

    current_user.hashed_password = get_password_hash(body.new_password)
    session.commit()
    return Message(message="Password updated successfully")


@router.get("/me", response_model=UserOut)
def read_user_me(current_user: User = Depends(get_current_user)) -> User:
    """
    Get current user.
    """
    return current_user


@router.delete("/me", response_model=Message)
def delete_user_me(
    session: Session = Depends(get_db), current_user: User = Depends(get_current_user)
) -> Any:
    """
    Delete own user.
    """
    if current_user.is_superuser:
        raise HTTPException(
            status_code=403, detail="Super users are not allowed to delete themselves"
        )

    # attach the user to their own session:
    to_delete_user = session.query(User).filter(User.id == current_user.id).one()

    session.delete(to_delete_user)
    session.commit()
    return Message(message="User deleted successfully")


@router.post("/signup", response_model=UserOut)
def register_user(
    user_in: UserRegister, session: Session = Depends(get_auth_db)
) -> UserOut:
    """
    Create new user without the need to be logged in.
    """
    user = crud.get_user_by_email(session=session, email=user_in.email)

    if user:
        raise HTTPException(
            status_code=400,
            detail="The user with this email already exists in the system",
        )

    user_create = UserRegister(
        full_name=user_in.full_name,
        email=user_in.email,
        password=user_in.password,
    )
    user = crud.create_user(session=session, user=user_create)

    send_telegram_message(
        message=f"User registered successfully {user_in.email} {user.id}",
    )
    return UserOut(
        full_name=user.full_name or "no name user",
        email=user.email,
        id=user.id,
        is_superuser=user.is_superuser,
        settings=user.settings,
    )


@router.get("/{user_id}", response_model=UserOut)
def read_user_by_id(
    user_id: uuid.UUID,
    session: Session = Depends(get_db),
    current_user: User = Depends(get_current_user),
) -> Any:
    """
    Get a specific user by id.
    """
    user = session.get(User, user_id)
    if not user:
        raise HTTPException(status_code=404, detail="User not found")
    if user != current_user and not current_user.is_superuser:
        raise HTTPException(
            status_code=403,
            detail="The user doesn't have enough privileges",
        )
    return user

