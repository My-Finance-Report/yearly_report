import os
from typing import Generator, Any
from dotenv import load_dotenv
from sqlalchemy import create_engine
import sqlalchemy.orm as orm

load_dotenv()

DATABASE_URL = os.environ["DATABASE_URL"]
print(f"****** {DATABASE_URL}")

engine = create_engine(DATABASE_URL, echo=True)

session_maker = orm.sessionmaker(autocommit=False, autoflush=False, bind=engine)
type Session = orm.Session


def get_db() -> Generator[Session, Any, None]:
    db = session_maker()
    try:
        yield db
    finally:
        db.close()
