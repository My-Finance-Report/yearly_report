import os
from dotenv import load_dotenv
from sqlalchemy import create_engine
import sqlalchemy.orm as orm

load_dotenv()

DATABASE_URL ="postgresql://persistent_user:persistent_pass@localhost:5433/persistent_db" # os.environ["DATABASE_URL"]
print(f"****** {DATABASE_URL}")

engine = create_engine(DATABASE_URL, echo=True)

session_maker = orm.sessionmaker(autocommit=False, autoflush=False, bind=engine)
Session = orm.Session


def get_db():
    db = session_maker()
    try:
        yield db
    finally:
        db.close()
