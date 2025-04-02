import asyncio
import logging
import time
from collections import defaultdict
from collections.abc import Callable, Coroutine
from datetime import datetime, timedelta, timezone
from typing import Any

from sqlalchemy import create_engine
from sqlalchemy.exc import PendingRollbackError
from sqlalchemy.orm import Session, sessionmaker

from app.async_pipelines.uploaded_file_pipeline.local_types import InProcessFile
from app.async_pipelines.uploaded_file_pipeline.main import uploaded_file_pipeline
from app.db import get_db_for_user
from app.get_db_string import get_worker_database_url
from app.models import JobKind, JobStatus, ProcessFileJob, UploadedPdf, User
from app.scheduler import sync_all_plaid_accounts_job
from app.telegram_utils import send_telegram_message

from ..async_pipelines.recategorize_pipeline.main import recategorize_file_pipeline

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

DATABASE_URL = get_worker_database_url()

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


def fetch_users_other_jobs(session: Session, user_id: int) -> list[ProcessFileJob]:
    jobs = (
        session.query(ProcessFileJob)
        .filter(
            ProcessFileJob.status == JobStatus.pending,
            ProcessFileJob.attempt_count < MAX_ATTEMPTS,
            ProcessFileJob.archived.is_(False),
            ProcessFileJob.user_id == user_id,
        )
        .limit(6)
        .all()
    )
    for job in jobs:
        job.status = JobStatus.processing
        job.last_tried_at = datetime.now(timezone.utc)
        job.attempt_count += 1

    session.commit()

    print(f"fetched {len(jobs)} more jobs")
    return jobs


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


def process_next_jobs(session: Session) -> None:
    all_jobs = []

    job = fetch_and_lock_next_job(session)
    if not job:
        logger.info(f"{datetime.now(timezone.utc)}: No job available.")
        return

    all_jobs.append(job)
    all_jobs.extend(fetch_users_other_jobs(session, job.user_id))

    job_ids = [job.id for job in all_jobs]

    user_session = create_user_specific_session(job.user_id)

    all_user_jobs = (
        user_session.query(ProcessFileJob).filter(ProcessFileJob.id.in_(job_ids)).all()
    )

    assert all(job.user_id == job.user_id for job in all_user_jobs), (
        "all jobs in batch must belong to one user"
    )

    logger.info(f"Processing jobs: {[job.id for job in all_user_jobs]}")
    success = try_jobs(user_session, all_user_jobs)

    for job in all_user_jobs:
        job.status = JobStatus.completed if success else JobStatus.failed
        job.last_tried_at = datetime.now(timezone.utc)
        user_session.add(job)
    user_session.commit()

    logger.info(
        f"Jobs completed with status: {','.join([job.status for job in all_user_jobs])}"
    )


def try_jobs(user_session: Session, jobs: list[ProcessFileJob]) -> bool:
    try:
        asyncio.run(run_jobs(user_session, jobs))

        for job in jobs:
            job.error_messages = ""
            user_session.add(job)
        user_session.commit()
        return True
    except (Exception, PendingRollbackError) as e:
        send_telegram_message(f"Job failed in worker: {e}")
        logger.error(f"Job failed: {e}")
        for job in jobs:
            job.status = JobStatus.failed
            user_session.add(job)
        user_session.commit()

        return False


FUNC_LOOKUP: dict[
    JobKind, Callable[[list[InProcessFile]], Coroutine[Any, Any, None]]
] = {
    JobKind.full_upload: uploaded_file_pipeline,
    JobKind.recategorize: recategorize_file_pipeline,
}


def create_user_specific_session(user_id: int) -> Session:
    return next(get_db_for_user(user_id))


async def run_jobs(_user_session: Session, jobs: list[ProcessFileJob]) -> None:
    in_process_files: dict[JobKind, list[InProcessFile]] = defaultdict(list)

    for job in jobs:
        job_specific_session = create_user_specific_session(job.user_id)
        specific_job = (
            job_specific_session.query(ProcessFileJob)
            .filter(ProcessFileJob.id == job.id)
            .one()
        )

        pdf = job_specific_session.get(UploadedPdf, specific_job.pdf_id)
        if not pdf:
            raise ValueError("PDF record not found!")

        user = job_specific_session.get(User, specific_job.user_id)
        if not user:
            raise ValueError("User record not found!")
        in_process_files[specific_job.kind].append(
            InProcessFile(
                session=job_specific_session, user=user, file=pdf, job=specific_job
            )
        )

    callable = FUNC_LOOKUP[job.kind]
    await callable(in_process_files[job.kind])


def worker() -> None:
    iterations = 0
    while True:
        with SessionLocal() as session:
            reset_stuck_jobs(session)
            process_next_jobs(session)
        time.sleep(POLL_INTERVAL)
        if iterations % 60 == 0:
            iterations = 0
            asyncio.run(sync_all_plaid_accounts_job())
        iterations += 1


if __name__ == "__main__":
    worker()
