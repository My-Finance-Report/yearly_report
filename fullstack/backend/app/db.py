import os
from collections.abc import Generator
from typing import Any

import sqlalchemy.orm as orm
from dotenv import load_dotenv
from sqlalchemy import create_engine

load_dotenv()

DATABASE_URL = os.environ["DATABASE_URL"]

engine = create_engine(DATABASE_URL, echo=True)

session_maker = orm.sessionmaker(autocommit=False, autoflush=False, bind=engine)
type Session = orm.Session


def get_db() -> Generator[Session, Any, None]:
    db = session_maker()
    try:
        yield db
    finally:
        db.close()
