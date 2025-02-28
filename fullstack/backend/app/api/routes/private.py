from typing import Any

from fastapi import APIRouter, Depends
from pydantic import BaseModel

from app.core.security import get_password_hash
from app.db import Session, get_db
from app.local_types import UserOut
from app.models import (
    User,
)

router = APIRouter(tags=["private"], prefix="/private")


class PrivateUserCreate(BaseModel):
    email: str
    password: str
    full_name: str
    is_verified: bool = False


@router.post("/users/", response_model=UserOut)
def create_user(user_in: PrivateUserCreate, session: Session = Depends(get_db)) -> Any:
    """
    Create a new user.
    """

    user = User(
        email=user_in.email,
        full_name=user_in.full_name,
        hashed_password=get_password_hash(user_in.password),
    )

    session.add(user)
    session.commit()

    return user
