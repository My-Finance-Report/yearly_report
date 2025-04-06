from pydantic import BaseModel


class Enable2FARequest(BaseModel):
    """Request to start the 2FA setup process."""

    password: str
    temp_token: str


class Enable2FAResponse(BaseModel):
    """Response containing the TOTP secret and QR code."""

    secret: str
    qr_code: str  # Base64 encoded QR code image


class Verify2FARequest(BaseModel):
    """Request to verify a 2FA code."""

    code: str
    temp_token: str


class Verify2FAResponse(BaseModel):
    """Response after verifying a 2FA code."""

    success: bool
    access_token: str
    token_type: str


class TwoFactorLoginRequest(BaseModel):
    """Request to complete login with 2FA."""

    code: str
    temp_token: str


class TwoFactorRejectRequest(BaseModel):
    """Request to reject 2FA setup during login."""

    temp_token: str
