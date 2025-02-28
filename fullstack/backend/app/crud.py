from app.core.security import get_password_hash, verify_password
from app.db import Session
from app.local_types import UserRegister, UserUpdate
from app.models import User


def get_user_by_email(*, session: Session, email: str) -> User | None:
    user = session.query(User).filter(User.email == email).first()
    return user


def create_user(*, session: Session, user: UserRegister) -> User:
    existing_user = session.query(User).filter(User.email == user.email).first()
    if existing_user:
        return existing_user

    new_user = User(
        full_name=user.full_name,
        hashed_password=get_password_hash(user.password),
        email=user.email,
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


def authenticate(*, session: Session, email: str, password: str) -> User | None:
    db_user = get_user_by_email(session=session, email=email)

    if not db_user:
        return None
    if not verify_password(password, db_user.hashed_password):
        return None
    return db_user
