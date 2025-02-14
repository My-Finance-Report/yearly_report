import os
import logging
import time
from datetime import datetime, timedelta, timezone
from typing import Optional

from app.uploaded_file_pipeline.main import uploaded_file_pipeline
from sqlalchemy import create_engine, select
from sqlalchemy.orm import sessionmaker, Session

from app.models import ProcessFileJob, UploadedPdf, User
from app.uploaded_file_pipeline.transaction_parser import InProcessFile

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

DATABASE_URL = os.getenv("DATABASE_URL")
engine = create_engine(DATABASE_URL, echo=True)

SessionLocal = sessionmaker(bind=engine, autoflush=False, autocommit=False)

POLL_INTERVAL = 10 
MAX_ATTEMPTS = 5


def reset_stuck_jobs(session: Session)->None:
    timeout = timedelta(minutes=3)
    now = datetime.now(timezone.utc)

    logger.info("Resetting stuck jobs...")
    session.execute(
        ProcessFileJob.__table__.update()
        .where(
            ProcessFileJob.status == "processing",
            ProcessFileJob.last_tried_at < now - timeout,
        )
        .values(status="pending")
    )
    session.commit()


def fetch_and_lock_next_job(session: Session) -> Optional[ProcessFileJob]:
    job = session.query(ProcessFileJob).filter(
        ProcessFileJob.status == "pending",
        ProcessFileJob.attempt_count < MAX_ATTEMPTS,
        ProcessFileJob.archived.is_(False),
    ).first()

    if job:
        job.status = "processing"
        job.last_tried_at = datetime.utcnow()
        job.attempt_count += 1
        session.commit()

        return job
    return None


def process_next_job(session: Session)->None:
    job = fetch_and_lock_next_job(session)

    if not job:
        logger.info("No jobs available.")
        return

    logger.info(f"Processing job: {job.id}")
    success = try_job(session, job)

    job.status = "completed" if success else "failed"
    job.last_tried_at = datetime.utcnow()
    session.commit()

    logger.info(f"Job {job.id} completed with status: {job.status}")


def try_job(session: Session, job: ProcessFileJob) -> bool:
    try:
        run_job(session, job)
        return True
    except Exception as e:
        logger.error(f"Job {job.id} failed: {e}")
        return False


def run_job(session: Session, job: ProcessFileJob)-> None:
    pdf = session.get(UploadedPdf, job.pdf_id)
    if not pdf:
        raise ValueError("PDF record not found!")

    user = session.get(User, job.user_id)
    if not user:
        raise ValueError("User record not found!")

    in_process = InProcessFile(session=session, user=user, file=pdf, job=job)
    result = uploaded_file_pipeline(in_process=in_process)

    if result:
        raise ValueError(result)


def worker()-> None:
    logger.info("Starting worker task...")

    while True:
        with SessionLocal() as session:
            reset_stuck_jobs(session)
            process_next_job(session)
        time.sleep(POLL_INTERVAL)  


if __name__ == "__main__":
    worker()
