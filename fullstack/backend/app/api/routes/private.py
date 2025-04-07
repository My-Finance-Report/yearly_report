from typing import Any

from fastapi import APIRouter, Depends, Query, Response
from pydantic import BaseModel

from app.api.routes.oauth import after_google_portion_of_auth
from app.core.config import get_env
from app.core.security import get_password_hash
from app.db import Session, get_auth_db, get_db
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


@router.get("/oauth/google/callback-local")
async def google_callback_local(
    code: str = Query(...),
    error: str | None = Query(None),
    session: Session = Depends(get_auth_db),
) -> Response:
    """
    Handle the callback from Google OAuth.
    This endpoint is called by the frontend after receiving the code from Google.
    """
    if get_env() != "local":
        return Response(status_code=404)

    if get_env() == "production":
        return Response(status_code=404)

    if error:
        return Response(content=error, status_code=400)

    try:
        user = session.query(User).filter(User.id == int(code)).first()

        if not user:
            return Response(content="User not found", status_code=404)

        return after_google_portion_of_auth(user)

    except Exception as e:
        return Response(content=str(e), status_code=500)
