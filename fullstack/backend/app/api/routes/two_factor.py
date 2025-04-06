import io
import base64
import pyotp
import qrcode
import jwt
import json
from fastapi import APIRouter, Depends, HTTPException, status, Response
from sqlalchemy.orm import Session
from datetime import timedelta
from jwt.exceptions import InvalidTokenError
from pydantic import ValidationError

from app import crud
from app.core import security
from app.core.config import settings
from app.local_types import Token, TokenPayload
from app.db import get_auth_db, get_current_user, get_current_user_from_temp_token
from app.models import User
from app.schemas.two_factor import (
    Enable2FARequest,
    Enable2FAResponse,
    TwoFactorRejectRequest,
    Verify2FARequest,
    Verify2FAResponse,
    TwoFactorLoginRequest,
)
from app.telegram_utils import send_telegram_message

router = APIRouter(prefix="/two-factor", tags=["two-factor"])


def make_token_and_respond(user_id:int)-> Response:
        
    # Create access token
    access_token_expires = timedelta(minutes=settings.ACCESS_TOKEN_EXPIRE_MINUTES)
    access_token = security.create_access_token(
        user_id, expires_delta=access_token_expires
    )

    # Create response data
    response_data = {
        "access_token": access_token,
        "token_type": "bearer",
    }
    
    # Create response object
    response = Response(
        content=json.dumps(response_data),
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
        path="/"
    )
    
    return response




@router.post("/enable", response_model=Enable2FAResponse)
def enable_2fa(
    request: Enable2FARequest,
    session: Session = Depends(get_auth_db),
):
    """Start the 2FA setup process"""
    current_user: User = get_current_user_from_temp_token(token=request.temp_token, session=session)
    if current_user.totp_enabled:
        raise HTTPException(status_code=400, detail="2FA is already enabled")
    
    # Generate secret
    secret = pyotp.random_base32()
    
    # Store the secret
    crud.enable_2fa(session=session, user_id=current_user.id, secret=secret)
    
    # Generate QR code
    totp = pyotp.TOTP(secret)
    provisioning_uri = totp.provisioning_uri(
        current_user.email,
        issuer_name="My Finance"
    )
    
    # Create QR code
    qr = qrcode.QRCode(version=1, box_size=10, border=5)
    qr.add_data(provisioning_uri)
    qr.make(fit=True)
    img = qr.make_image(fill_color="black", back_color="white")
    
    # Convert to base64
    buffered = io.BytesIO()
    img.save(buffered, format="PNG")
    qr_code = base64.b64encode(buffered.getvalue()).decode()
    
    send_telegram_message(
        message=f"User {current_user.id} initiated 2FA setup",
    )
    
    return Enable2FAResponse(
        secret=secret,
        qr_code=f"data:image/png;base64,{qr_code}"
    )


@router.post("/verify", response_model=Verify2FAResponse)
def verify_2fa(
    request: Verify2FARequest,
    session: Session = Depends(get_auth_db),
) -> Response:

    current_user: User = get_current_user_from_temp_token(token=request.temp_token, session=session)
    if not current_user.totp_secret:
        raise HTTPException(status_code=400, detail="2FA is not set up")
    
    totp = pyotp.TOTP(current_user.totp_secret)
    if not totp.verify(request.code):
        raise HTTPException(status_code=400, detail="Invalid code")
    
    # Enable 2FA for the user
    crud.activate_2fa(session=session, user_id=current_user.id)
    
    # Create access token
    access_token_expires = timedelta(minutes=settings.ACCESS_TOKEN_EXPIRE_MINUTES)
    access_token = security.create_access_token(
        current_user.id, expires_delta=access_token_expires
    )
    
    # Create response data
    response_data = {
        "success": True,
        "access_token": access_token,
        "token_type": "bearer",
    }
    
    # Create response object
    response = Response(
        content=json.dumps(response_data),
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
        path="/"
    )
    
    return response


@router.post("/verify-login", response_model=Token)
def verify_2fa_login(
    request: TwoFactorLoginRequest,
    session: Session = Depends(get_auth_db),
) -> Response:
    """Verify the 2FA code during login"""
    # Extract the user ID from the temporary token
    try:
        payload = jwt.decode(
            request.temp_token, settings.SECRET_KEY, algorithms=[security.ALGORITHM]
        )
        token_data = TokenPayload(**payload)
        user_id = token_data.sub
    except (InvalidTokenError, ValidationError):
        raise HTTPException(
            status_code=status.HTTP_403_FORBIDDEN,
            detail="Could not validate temporary token",
        )
    
    # Get the user
    user = session.query(User).filter(User.id == user_id).first()
    if not user:
        raise HTTPException(status_code=404, detail="User not found")
    if not user.is_active:
        raise HTTPException(status_code=400, detail="Inactive user")
    
    # Verify the 2FA code
    if not user.totp_secret or not user.totp_enabled:
        raise HTTPException(status_code=400, detail="2FA is not enabled for this user")
    
    totp = pyotp.TOTP(user.totp_secret)
    if not totp.verify(request.code):
        raise HTTPException(status_code=400, detail="Invalid code")
    
    
    
    send_telegram_message(
        message=f"User {user.id} successfully verified 2FA during login",
    )

    return make_token_and_respond(user_id=user.id)



@router.post("/reject-tfa", response_model=Token)
def reject_2fa(
    request: TwoFactorRejectRequest,
    session: Session = Depends(get_auth_db),
) -> Response:
    """Reject the 2FA setup during login"""
    # Extract the user ID from the temporary token
    try:
        payload = jwt.decode(
            request.temp_token, settings.SECRET_KEY, algorithms=[security.ALGORITHM]
        )
        token_data = TokenPayload(**payload)
        user_id = token_data.sub
    except (InvalidTokenError, ValidationError):
        raise HTTPException(
            status_code=status.HTTP_403_FORBIDDEN,
            detail="Could not validate temporary token",
        )
    
    # Get the user
    user = session.query(User).filter(User.id == user_id).first()
    if not user:
        raise HTTPException(status_code=404, detail="User not found")
    if not user.is_active:
        raise HTTPException(status_code=400, detail="Inactive user")
    
    # Verify they dont have 2FA enabled
    if user.totp_secret or user.totp_enabled:
        raise HTTPException(status_code=400, detail="2FA is already enabled for this user")

    user.totp_enabled=False
    user.totp_secret=None
    user.requires_two_factor=False
    session.commit()
    
        
    send_telegram_message(
        message=f"User {user.id} rejected 2FA during login",
    )

    return make_token_and_respond(user_id=user.id)
    
@router.post("/disable", response_model=Verify2FAResponse)
def disable_2fa(
    request: Enable2FARequest,
    current_user: User = Depends(get_current_user),
    session: Session = Depends(get_auth_db),
) -> Verify2FAResponse:
    """Disable 2FA for a user"""
    if not current_user.totp_enabled:
        raise HTTPException(status_code=400, detail="2FA is not enabled")
    
    if not crud.verify_password(request.password, current_user.hashed_password):
        raise HTTPException(status_code=400, detail="Invalid password")
    
    crud.disable_2fa(session=session, user_id=current_user.id)
    
    send_telegram_message(
        message=f"User {current_user.id} disabled 2FA",
    )
    
    return Verify2FAResponse(success=True, access_token="", token_type="bearer")
