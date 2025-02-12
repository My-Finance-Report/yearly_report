from collections.abc import Generator
from tokenize import Token
from typing import Annotated

import jwt
from fastapi import Depends, HTTPException, status
from fastapi.security import OAuth2PasswordBearer
from jwt.exceptions import InvalidTokenError
from pydantic import ValidationError
from sqlalchemy.orm import  sessionmaker

from app.core import security
from app.core.config import settings
from app.core.db import engine
from app.models import User
from app.db import get_db, Session
from app.local_types import TokenPayload

SessionLocal = sessionmaker(autocommit=False, autoflush=False, bind=engine)

reusable_oauth2 = OAuth2PasswordBearer(
    tokenUrl=f"{settings.API_V1_STR}/login/access-token"
)



# Dependency to get the current authenticated user
def get_current_user(token=Depends(reusable_oauth2),session:Session= Depends(get_db)) -> User:
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




def get_current_active_superuser(current_user: User = Depends(get_current_user)) -> User:
    if not current_user.is_superuser:
        raise HTTPException(
            status_code=403, detail="The user doesn't have enough privileges"
        )
    return current_user
