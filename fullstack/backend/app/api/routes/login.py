import json
from typing import Annotated, Any

from fastapi import APIRouter, Depends, HTTPException, Response
from fastapi.responses import HTMLResponse
from fastapi.security import OAuth2PasswordRequestForm

from app import crud
from app.core.security import get_password_hash
from app.db import Session, get_auth_db, get_current_active_superuser, get_current_user
from app.local_types import Message, NewPassword, Token, UserOut
from app.models.user import User
from app.telegram_utils import send_telegram_message
from app.utils import (
    generate_password_reset_token,
    generate_reset_password_email,
    verify_password_reset_token,
)
from app.email.send import send_email

router = APIRouter(tags=["login"])


@router.post("/login/access-token", response_model=Token)
def login_access_token(
    form_data: Annotated[OAuth2PasswordRequestForm, Depends()],
    session: Session = Depends(get_auth_db),
) -> Response:
    """
    OAuth2 compatible token login, get an access token for future requests
    """
    send_telegram_message(
        message=f"User logged in attempt {form_data.username}",
    )

    user_in_progress = crud.authenticate(
        session=session, email=form_data.username, password=form_data.password
    )

    if not user_in_progress:
        send_telegram_message(
            message="User failed to log in",
        )
        # Don't reveal whether the email exists or password is wrong
        raise HTTPException(status_code=401, detail="Invalid credentials")

    return crud.handle_and_respond_to_in_progress_login(user_in_progress, "basic")


@router.post("/logout")
def logout() -> Response:
    """
    Logout endpoint that clears the access token cookie
    """
    response = Response(
        content=json.dumps({"message": "Successfully logged out"}),
        media_type="application/json",
    )

    response.delete_cookie(
        key="access_token", path="/", domain=None, secure=False, samesite="lax"
    )

    return response


@router.post("/login/test-token", response_model=UserOut)
def test_token(current_user: User = Depends(get_current_user)) -> User:
    """
    Test access token
    """
    return current_user


@router.post("/password-recovery/{email}")
def recover_password(email: str, session: Session = Depends(get_auth_db)) -> Message:
    """
    Password Recovery
    """
    user = crud.get_user_by_email(session=session, email=email)

    send_telegram_message(
        message=f"hit hard wall on password recovery",
    )
    raise HTTPException(
        status_code=404,
        detail="The user with this email does not exist in the system.",
    )
    """
    password_reset_token = generate_password_reset_token(email=email)
    email_data = generate_reset_password_email(
        email_to=user.email, email=email, token=password_reset_token
    )
    send_email(user, lambda: email_data)
    
    return Message(message="Password recovery email sent")

    """


@router.post("/reset-password/")
def reset_password(
    body: NewPassword, session: Session = Depends(get_auth_db)
) -> Message:
    """
    Reset password
    """
    if not body.token.access_token:
        raise HTTPException(status_code=400, detail="Invalid token")
    email = verify_password_reset_token(token=body.token.access_token)
    if not email:
        raise HTTPException(status_code=400, detail="Invalid token")
    user = crud.get_user_by_email(session=session, email=email)
    if not user:
        raise HTTPException(
            status_code=404,
            detail="The user with this email does not exist in the system.",
        )
    elif not user.is_active:
        raise HTTPException(status_code=400, detail="Inactive user")
    hashed_password = get_password_hash(password=body.new_password)
    user.hashed_password = hashed_password
    session.add(user)
    session.commit()
    return Message(message="Password updated successfully")


@router.post(
    "/password-recovery-html-content/{email}",
    dependencies=[Depends(get_current_active_superuser)],
    response_class=HTMLResponse,
)
def recover_password_html_content(
    email: str, session: Session = Depends(get_auth_db)
) -> Any:
    """
    HTML Content for Password Recovery
    """
    user = crud.get_user_by_email(session=session, email=email)

    if not user:
        raise HTTPException(
            status_code=404,
            detail="The user with this username does not exist in the system.",
        )
    password_reset_token = generate_password_reset_token(email=email)
    email_data = generate_reset_password_email(
        email_to=user.email, email=email, token=password_reset_token
    )

    return HTMLResponse(
        content=email_data.html_content, headers={"subject:": email_data.subject}
    )
