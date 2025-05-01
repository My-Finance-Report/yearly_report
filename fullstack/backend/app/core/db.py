import sqlalchemy.orm as orm
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker

from app import crud
from app.core.config import settings
from app.get_db_string import get_app_user_database_url
from app.models.user import User

from ..local_types import UserRegister

# Create SQLAlchemy engine
DATABASE_URL = get_app_user_database_url()
engine = create_engine(DATABASE_URL, pool_pre_ping=True)

# Create session factory
SessionLocal = sessionmaker(autocommit=False, autoflush=False, bind=engine)

type Session = orm.Session


def init_db(session: Session) -> None:
    """Initialize the database and create a superuser if not exists."""

    # Check if the superuser already exists
    user = session.query(User).filter(User.email == settings.FIRST_SUPERUSER).first()

    if not user:
        user_in = UserRegister(
            email=settings.FIRST_SUPERUSER,
            password=settings.FIRST_SUPERUSER_PASSWORD,
            is_superuser=True,
            is_active=True,
            full_name="first superuser",
        )
        user = crud.create_user(session=session, user=user_in)

    session.commit()
