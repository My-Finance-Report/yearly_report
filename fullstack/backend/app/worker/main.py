import asyncio
import enum
import logging
import time
import uuid
from collections import defaultdict
from collections.abc import Callable, Coroutine
from datetime import datetime, timedelta, timezone
from typing import Any

from sqlalchemy import create_engine
from sqlalchemy.exc import PendingRollbackError
from sqlalchemy.orm import Session, sessionmaker

from app.async_pipelines.recategorize_plaid_pipeline.main import (
    recategorize_account_pipeline,
)
from app.async_pipelines.uploaded_file_pipeline.local_types import InProcessJob
from app.async_pipelines.uploaded_file_pipeline.main import uploaded_file_pipeline
from app.db import get_db_for_user
from app.get_db_string import get_worker_database_url
from app.models.plaid import PlaidSyncLog
from app.models.uploaded_pdf import UploadedPdf
from app.models.worker_job import JobKind, JobStatus, WorkerJob
from app.models.user import User
from app.models.worker_status import WorkerStatus
from app.no_code.notifications.events import (
    DailyEvent,
    Event,
    MonthlyEvent,
    WeeklyEvent,
)
from app.no_code.notifications.trigger import trigger_effects
from app.scheduler import sync_all_plaid_accounts_job
from app.telegram_utils import send_telegram_message

from ..async_pipelines.recategorize_pipeline.main import recategorize_file_pipeline
import os
import certifi

os.environ["SSL_CERT_FILE"] = certifi.where()

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
        session.query(WorkerJob)
        .filter(
            WorkerJob.status == "processing",
            WorkerJob.last_tried_at < now - timeout,
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


def fetch_users_other_jobs(session: Session, user_id: int) -> list[WorkerJob]:
    jobs = (
        session.query(WorkerJob)
        .filter(
            WorkerJob.status == JobStatus.pending,
            WorkerJob.attempt_count < MAX_ATTEMPTS,
            WorkerJob.archived.is_(False),
            WorkerJob.user_id == user_id,
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


def fetch_and_lock_next_job(session: Session) -> WorkerJob | None:
    job = (
        session.query(WorkerJob)
        .filter(
            WorkerJob.status == "pending",
            WorkerJob.attempt_count < MAX_ATTEMPTS,
            WorkerJob.archived.is_(False),
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
        user_session.query(WorkerJob).filter(WorkerJob.id.in_(job_ids)).all()
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


def try_jobs(user_session: Session, jobs: list[WorkerJob]) -> bool:
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
    JobKind, Callable[[list[InProcessJob]], Coroutine[Any, Any, None]]
] = {
    JobKind.full_upload: uploaded_file_pipeline,
    JobKind.recategorize: recategorize_file_pipeline,
    JobKind.plaid_recategorize: recategorize_account_pipeline,
}


def create_user_specific_session(user_id: int) -> Session:
    return next(get_db_for_user(user_id))


async def run_jobs(_user_session: Session, jobs: list[WorkerJob]) -> None:
    in_process_files: dict[JobKind, list[InProcessJob]] = defaultdict(list)

    for job in jobs:
        job_specific_session = create_user_specific_session(job.user_id)
        specific_job = (
            job_specific_session.query(WorkerJob).filter(WorkerJob.id == job.id).one()
        )

        if specific_job.pdf_id is not None:
            pdf = job_specific_session.get(UploadedPdf, specific_job.pdf_id)
            if not pdf:
                raise ValueError("PDF record not found!")
        else:
            pdf = None

        user = job_specific_session.get(User, specific_job.user_id)
        if not user:
            raise ValueError("User record not found!")

        batch_uuid = uuid.uuid4().hex
        in_process_files[specific_job.kind].append(
            InProcessJob(
                session=job_specific_session,
                user=user,
                file=pdf,
                job=specific_job,
                batch_id=batch_uuid,
            )
        )

    callable = FUNC_LOOKUP[job.kind]
    await callable(in_process_files[job.kind])


def handle_plaid() -> None:
    try:
        asyncio.run(sync_all_plaid_accounts_job())
    except Exception as e:
        send_telegram_message(f"Failed to sync Plaid accounts: {e}")


def clean_worker_status() -> None:
    with SessionLocal() as session:
        session.query(WorkerStatus).filter(
            WorkerStatus.created_at < datetime.now(timezone.utc) - timedelta(days=2)
        ).delete()
        session.commit()


def clean_plaid_sync_logs() -> None:
    with SessionLocal() as session:
        session.query(PlaidSyncLog).filter(
            PlaidSyncLog.created_at < datetime.now(timezone.utc) - timedelta(days=2)
        ).delete()
        session.commit()


def upload_file_worker() -> None:
    with SessionLocal() as session:
        reset_stuck_jobs(session)
        process_next_jobs(session)


def fire_timed_event(event: Event) -> None:
    with SessionLocal() as session:
        users = session.query(User).all()
        for user in users:
            user_session = next(get_db_for_user(user.id))
            trigger_effects(user_session, user, event)
            user_session.close()


def fire_weekly_event() -> None:
    fire_timed_event(WeeklyEvent())


def fire_monthly_event() -> None:
    fire_timed_event(MonthlyEvent())


def fire_daily_event() -> None:
    fire_timed_event(DailyEvent())


class Frequency(str, enum.Enum):
    every_20_seconds = "every_20_seconds"
    every_minute = "every_minute"
    every_hour = "every_hour"
    every_day_at_8am = "every_day_at_8am"
    every_week_monday_at_8am = "every_week_monday_at_8am"
    every_month_1st_at_8am = "every_month_1st_at_8am"

    @property
    def seconds(self) -> int:
        minute = 60
        hour = 60 * minute
        day = 24 * hour
        week = 7 * day
        month = 30 * day  # YOLO

        return {
            self.every_20_seconds: 20,
            self.every_minute: minute,
            self.every_hour: hour,
            self.every_day_at_8am: day,
            self.every_week_monday_at_8am: week,
            self.every_month_1st_at_8am: month,
        }[self]

    def should_run_at(self, now: datetime) -> bool:
        """Check if this frequency should run at the given time"""
        # Regular interval jobs can always run
        if self in [self.every_20_seconds, self.every_minute, self.every_hour]:
            return True

        # All time-specific jobs run at 8am UTC
        if now.hour != 8 or now.minute != 0:
            return False

        if self == self.every_day_at_8am:
            return True

        if self == self.every_week_monday_at_8am:
            return now.weekday() == 0  # Monday

        if self == self.every_month_1st_at_8am:
            return now.day == 1

        return False


CRONS: dict[Frequency, list[Callable[[], None]]] = {
    Frequency.every_20_seconds: [upload_file_worker],
    Frequency.every_minute: [handle_plaid],
    Frequency.every_hour: [clean_worker_status, clean_plaid_sync_logs],
    Frequency.every_day_at_8am: [fire_daily_event],
    Frequency.every_week_monday_at_8am: [fire_weekly_event],
    Frequency.every_month_1st_at_8am: [fire_monthly_event],
}


def should_run_job(session: Session, job_name: str, frequency: Frequency) -> bool:
    from app.models.cron_state import CronState

    now = datetime.now(timezone.utc)
    state = session.query(CronState).filter(CronState.job_name == job_name).first()

    if not state:
        # First time this job has ever run
        state = CronState(job_name=job_name, last_run=now)
        session.add(state)
        session.commit()
        return frequency.should_run_at(now)

    # For time-specific jobs, check if we're in the right time window
    if not frequency.should_run_at(now):
        return False

    time_since_last_run = now - state.last_run
    # For time-specific jobs, ensure we haven't run today yet
    if frequency in [
        Frequency.every_day_at_8am,
        Frequency.every_week_monday_at_8am,
        Frequency.every_month_1st_at_8am,
    ]:
        return bool(
            state.last_run.date() < now.date()
            and time_since_last_run.total_seconds() >= 60
        )

    # For regular interval jobs, use the seconds property
    should_run = time_since_last_run.total_seconds() >= frequency.seconds

    if should_run:
        session.query(CronState).filter(CronState.job_name == job_name).update(
            {CronState.last_run: now}
        )
        session.commit()

    return bool(should_run)


def worker() -> None:
    while True:
        with SessionLocal() as session:
            for freq, jobs in CRONS.items():
                if not jobs:
                    continue

                for job in jobs:
                    try:
                        if should_run_job(session, job.__name__, freq):
                            job()
                    except Exception as e:
                        logging.error(f"Error running {job.__name__}: {e}")

        time.sleep(5)


if __name__ == "__main__":
    worker()
