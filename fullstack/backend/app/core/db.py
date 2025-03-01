from sqlalchemy import create_engine
import sqlalchemy.orm as orm
from sqlalchemy.orm import sessionmaker

from app import crud
from app.core.config import settings
from app.models import User

from ..local_types import UserRegister

# Create SQLAlchemy engine
engine = create_engine(str(settings.SQLALCHEMY_DATABASE_URI), pool_pre_ping=True)

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
