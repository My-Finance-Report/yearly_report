import logging

from sqlalchemy import create_engine, text, Engine
from db import engine
from sqlalchemy.orm import sessionmaker
from tenacity import after_log, before_log, retry, stop_after_attempt, wait_fixed

from app.core.db import engine

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

# Retry settings
max_tries = 60 * 5  # Retry for up to 5 minutes
wait_seconds = 1

# Create a session factory
SessionLocal = sessionmaker(autocommit=False, autoflush=False, bind=engine)


@retry(
    stop=stop_after_attempt(max_tries),
    wait=wait_fixed(wait_seconds),
    before=before_log(logger, logging.INFO),
    after=after_log(logger, logging.WARN),
)
def init(db_engine: Engine) -> None:
    """Attempt to create a database session to check if the DB is awake."""
    try:
        with SessionLocal() as session:
            # Execute a simple query to check DB connection
            session.execute(text("SELECT 1"))
    except Exception as e:
        logger.error(f"Database initialization failed: {e}")
        raise e


def main() -> None:
    logger.info("Initializing service...")
    init(engine)
    logger.info("Service finished initializing.")


if __name__ == "__main__":
    main()
