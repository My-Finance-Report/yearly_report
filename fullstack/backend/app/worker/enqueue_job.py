from datetime import datetime, timezone

from sqlalchemy.orm import Session

from app.models import JobKind, JobStatus, UploadConfiguration, WorkerJob


def enqueue_recategorization(
    session: Session,
    user_id: int,
    transaction_source_id: int,
) -> None:
    query = (
        session.query(WorkerJob)
        .filter(
            WorkerJob.user_id == user_id,
            WorkerJob.status == JobStatus.completed,
        )
        .join(UploadConfiguration, UploadConfiguration.id == WorkerJob.config_id)
        .filter(UploadConfiguration.transaction_source_id == transaction_source_id)
    ).all()

    if not query:
        # things like plaid accounts dont have jobs already
        existing_config = (
            session.query(UploadConfiguration)
            .filter(
                UploadConfiguration.transaction_source_id == transaction_source_id,
                UploadConfiguration.user_id == user_id,
            )
            .one_or_none()
        )
        if not existing_config:
            new_config = UploadConfiguration(
                transaction_source_id=transaction_source_id,
                user_id=user_id,
                filename_regex=".*",
                start_keyword=None,
                end_keyword=None,
            )
            session.add(new_config)
            session.commit()

        new_job = WorkerJob(
            created_at=datetime.now(timezone.utc),
            last_tried_at=None,
            status=JobStatus.pending,
            user_id=user_id,
            config_id=existing_config.id if existing_config else new_config.id,
            kind=JobKind.plaid_recategorize,
            pdf_id=None,
            archived=False,
            attempt_count=0,
        )
        session.add(new_job)
        session.commit()
        return

    for job in query:
        job.kind = JobKind.recategorize
        job.attempt_count = 0
        job.status = JobStatus.pending
        session.add(job)

    session.commit()


def enqueue_or_reset_job(
    session: Session,
    user_id: int,
    pdf_id: int | None,
    job_kind: JobKind,
) -> WorkerJob:
    existing_job = (
        session.query(WorkerJob)
        .filter(WorkerJob.pdf_id == pdf_id, WorkerJob.user_id == user_id)
        .one_or_none()
    )

    job: WorkerJob
    if existing_job:
        existing_job.kind = (
            job_kind
            if existing_job.status == JobStatus.completed
            else JobKind.full_upload
        )
        existing_job.status = JobStatus.pending
        existing_job.attempt_count = 0
        session.add(existing_job)
        session.commit()
        job = existing_job

    else:
        new_job = WorkerJob(
            created_at=datetime.now(timezone.utc),
            last_tried_at=None,
            status=JobStatus.pending,
            user_id=user_id,
            config_id=None,
            kind=job_kind,
            pdf_id=pdf_id,
            archived=False,
            attempt_count=0,
        )

        session.add(new_job)
        session.commit()
        job = new_job

    return job


def enqueue_or_reset_jobs(
    session: Session,
    user_id: int,
    pdf_ids: list[int],
    job_kind: JobKind,
) -> list[WorkerJob]:
    out = []
    for pdf_id in pdf_ids:
        out.append(enqueue_or_reset_job(session, user_id, pdf_id, job_kind=job_kind))
    return out
