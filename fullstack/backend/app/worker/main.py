import os
import asyncio
import logging
from datetime import datetime, timedelta, timezone
from typing import Optional

from sqlalchemy.ext.asyncio import AsyncSession, create_async_engine
from sqlalchemy.future import select
from sqlalchemy.orm import sessionmaker

from app.models import ProcessFileJob, UploadedPdf, User
from app.parsers import InProcessFile, process_pdf_file

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

DATABASE_URL = os.getenv("DATABASE_URL")
engine = create_async_engine(DATABASE_URL, echo=True)
SessionLocal = sessionmaker(bind=engine, class_=AsyncSession, expire_on_commit=False)

POLL_INTERVAL = 10  # 10 seconds
MAX_ATTEMPTS = 5


async def reset_stuck_jobs(session: AsyncSession):
    timeout = timedelta(minutes=3)
    now = datetime.now(timezone.utc)

    logger.info("Resetting stuck jobs...")
    await session.execute(
        ProcessFileJob.__table__.update()
        .where(
            ProcessFileJob.status == "processing",
            ProcessFileJob.last_tried_at < now - timeout,
        )
        .values(status="pending")
    )
    await session.commit()


async def fetch_and_lock_next_job(session: AsyncSession) -> Optional[ProcessFileJob]:
    async with session.begin():
        result = await session.execute(
            select(ProcessFileJob)
            .where(
                ProcessFileJob.status == "pending",
                ProcessFileJob.attempt_count < MAX_ATTEMPTS,
                ProcessFileJob.archived.is_(False),
            )
            .limit(1)
        )
        job = result.scalars().first()

        if job:
            job.status = "processing"
            job.last_tried_at = datetime.utcnow()
            job.attempt_count += 1
            await session.commit()

            return job
    return None


async def process_next_job():
    async with SessionLocal() as session:
        job = await fetch_and_lock_next_job(session)

        if not job:
            logger.info("No jobs available.")
            return

        logger.info(f"Processing job: {job.id}")

        success = await try_job(session, job)
        job.status = "completed" if success else "failed"
        job.last_tried_at = datetime.utcnow()

        async with session.begin():
            await session.commit()

        logger.info(f"Job {job.id} completed with status: {job.status}")


async def try_job(session: AsyncSession, job: ProcessFileJob) -> bool:
    try:
        await run_job(session, job)
        return True
    except Exception as e:
        logger.error(f"Job {job.id} failed: {e}")
        return False


async def run_job(session: AsyncSession, job: ProcessFileJob):
    async with session.begin():
        pdf = await session.get(UploadedPdf, job.pdf_id)
        if not pdf:
            raise ValueError("PDF record not found!")

        user = await session.get(User, job.user_id)
        if not user:
            raise ValueError("User record not found!")

    in_process = InProcessFile(session=session, user=user, file=pdf, job=job)
    result = process_pdf_file(in_process=in_process)

    if result:
        raise ValueError(result)


async def worker():
    logger.info("Starting worker task...")

    while True:
        async with SessionLocal() as session:
            await reset_stuck_jobs(session)
            await process_next_job()
        await asyncio.sleep(POLL_INTERVAL)


if __name__ == "__main__":
    asyncio.run(worker())
