from datetime import timedelta
from typing import Annotated, Any

from fastapi import APIRouter, Depends, HTTPException, Response
from fastapi.responses import HTMLResponse, JSONResponse
from fastapi.security import OAuth2PasswordRequestForm

from app import crud
from app.core import security
from app.core.config import settings
from app.core.security import get_password_hash
from app.db import Session, get_auth_db, get_current_active_superuser, get_current_user
from app.local_types import Message, NewPassword, Token, UserOut
from app.models import User
from app.telegram_utils import send_telegram_message
from app.utils import (
    generate_password_reset_token,
    generate_reset_password_email,
    send_email,
    verify_password_reset_token,
)
import json

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

    if user_in_progress.requires_2fa or user_in_progress.requires_2fa_setup:
        send_telegram_message(
            message=f"User {form_data.username} needs 2FA verification",
        )
        # Return the 2FA challenge response with 200 status code
        response_data = {
                "access_token": None, 
                "token_type": "bearer", 
                "requires_2fa": user_in_progress.requires_2fa, 
                "requires_2fa_setup": user_in_progress.requires_2fa_setup,
                "temp_token": user_in_progress.temp_token
            }
        return Response(
            content=json.dumps(response_data),
            media_type="application/json"
        )

    # If we get here, user_in_progress is a User object
    user = user_in_progress.user
    
    if not user.is_active:
        raise HTTPException(status_code=401, detail="Account is inactive")

    access_token_expires = timedelta(minutes=settings.ACCESS_TOKEN_EXPIRE_MINUTES)
    access_token = security.create_access_token(
        user.id, expires_delta=access_token_expires
    )
    
    # Create response with token
    token_data = {
        "access_token": access_token,
        "token_type": "bearer",
    }
    
    # Create a Response object with the token data
    response = Response(
        content=json.dumps(token_data),
        media_type="application/json"
    )
    
    # Set HttpOnly cookie
    response.set_cookie(
        key="access_token",
        value=access_token,
        httponly=True,
        max_age=settings.ACCESS_TOKEN_EXPIRE_MINUTES * 60,
        expires=settings.ACCESS_TOKEN_EXPIRE_MINUTES * 60,
        samesite="lax",
        secure=False,
    )
    
    send_telegram_message(
        message=f"User logged in successfully {user.id}",
    )
    
    return response


@router.post("/logout")
def logout():
    """
    Logout endpoint that clears the access token cookie
    """
    response = Response(
        content=json.dumps({"message": "Successfully logged out"}),
        media_type="application/json"
    )
    
    response.delete_cookie(
        key="access_token",
        path="/",
        domain=None,
        secure=False,
        samesite="lax"
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

    if not user:
        raise HTTPException(
            status_code=404,
            detail="The user with this email does not exist in the system.",
        )
    password_reset_token = generate_password_reset_token(email=email)
    email_data = generate_reset_password_email(
        email_to=user.email, email=email, token=password_reset_token
    )
    send_email(
        email_to=user.email,
        subject=email_data.subject,
        html_content=email_data.html_content,
    )
    return Message(message="Password recovery email sent")


@router.post("/reset-password/")
def reset_password(
    body: NewPassword, session: Session = Depends(get_auth_db)
) -> Message:
    """
    Reset password
    """
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
