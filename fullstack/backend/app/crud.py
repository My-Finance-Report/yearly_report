from app.core.security import get_password_hash, verify_password
from app.db import Session
from app.local_types import UserRegister, UserUpdate
from app.models import User, UserSettings
from app.telegram_utils import send_telegram_message


def get_user_by_email(*, session: Session, email: str) -> User | None:
    user = session.query(User).filter(User.email == email).one_or_none()
    return user


def create_user(*, session: Session, user: UserRegister) -> User:
    existing_user = session.query(User).filter(User.email == user.email).one_or_none()
    if existing_user:
        raise ValueError("Email already registered")

    new_user = User(
        full_name=user.full_name,
        hashed_password=get_password_hash(user.password),
        email=user.email,
        settings=UserSettings(power_user_filters=True, has_budget=False),
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
    if db_user.requires_two_factor:
        send_telegram_message(
            message=f"User hit 2fa wall {db_user.id}",
        )
        raise NotImplementedError("todo")
    if not verify_password(password, db_user.hashed_password):
        print("hashed pw", db_user.hashed_password)
        return None
    return db_user
