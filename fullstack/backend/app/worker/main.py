import logging
import os
import time
from collections.abc import Callable
from datetime import datetime, timedelta, timezone

from sqlalchemy import create_engine
from sqlalchemy.orm import Session, sessionmaker

from app.async_pipelines.uploaded_file_pipeline.local_types import InProcessFile
from app.async_pipelines.uploaded_file_pipeline.main import uploaded_file_pipeline
from app.models import JobKind, JobStatus, ProcessFileJob, UploadedPdf, User

from ..async_pipelines.recategorize_pipeline.main import recategorize_pipeline

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

DATABASE_URL = os.environ["DATABASE_URL"]
engine = create_engine(DATABASE_URL)

SessionLocal = sessionmaker(bind=engine, autoflush=False, autocommit=False)

POLL_INTERVAL = 10
MAX_ATTEMPTS = 5


def reset_stuck_jobs(session: Session) -> None:
    timeout = timedelta(minutes=3)
    now = datetime.now(timezone.utc)

    logger.info("Resetting stuck jobs...")

    # Fetch stuck jobs
    stuck_jobs = (
        session.query(ProcessFileJob)
        .filter(
            ProcessFileJob.status == "processing",
            ProcessFileJob.last_tried_at < now - timeout,
        )
        .all()
    )

    if not stuck_jobs:
        logger.info("No stuck jobs found.")
        return

    for job in stuck_jobs:
        job.status = JobStatus.pending

    session.commit()

    logger.info(f"Reset {len(stuck_jobs)} stuck jobs.")


def fetch_and_lock_next_job(session: Session) -> ProcessFileJob | None:
    job = (
        session.query(ProcessFileJob)
        .filter(
            ProcessFileJob.status == "pending",
            ProcessFileJob.attempt_count < MAX_ATTEMPTS,
            ProcessFileJob.archived.is_(False),
        )
        .first()
    )

    if job:
        job.status = JobStatus.processing
        job.last_tried_at = datetime.now(timezone.utc)
        job.attempt_count += 1
        session.commit()

        return job
    return None


def process_next_job(session: Session) -> None:
    job = fetch_and_lock_next_job(session)

    if not job:
        logger.info("No jobs available.")
        return

    logger.info(f"Processing job: {job.id}")
    success = try_job(session, job)

    job.status = JobStatus.completed if success else JobStatus.failed
    job.last_tried_at = datetime.utcnow()
    session.commit()

    logger.info(f"Job {job.id} completed with status: {job.status}")


def try_job(session: Session, job: ProcessFileJob) -> bool:
    try:
        run_job(session, job)
        return True
    except Exception as e:
        error_message = f"{job.error_messages or ''}\n{e}"
        job.error_messages = error_message.strip()
        session.add(job)
        session.commit()

        logger.error(f"Job {job.id} failed: {e}")
        return False


FUNC_LOOKUP: dict[JobKind, Callable[[InProcessFile], None]] = {
    JobKind.full_upload: uploaded_file_pipeline,
    JobKind.recategorize: recategorize_pipeline,
}


def run_job(session: Session, job: ProcessFileJob) -> None:
    pdf = session.get(UploadedPdf, job.pdf_id)
    if not pdf:
        raise ValueError("PDF record not found!")

    user = session.get(User, job.user_id)
    if not user:
        raise ValueError("User record not found!")

    in_process = InProcessFile(session=session, user=user, file=pdf, job=job)

    callable = FUNC_LOOKUP[job.kind]
    callable(in_process)


def worker() -> None:
    while True:
        with SessionLocal() as session:
            reset_stuck_jobs(session)
            process_next_job(session)
        time.sleep(POLL_INTERVAL)


if __name__ == "__main__":
    worker()
