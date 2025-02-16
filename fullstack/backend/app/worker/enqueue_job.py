from datetime import datetime, timezone

from app.models import JobKind, JobStatus, ProcessFileJob, UploadConfiguration 
from sqlalchemy.orm import Session

def enqueue_recategorization(
    session: Session,
    user_id: int,
    transaction_source_id: int,
) -> None:

    query = session.query(ProcessFileJob).filter(ProcessFileJob.user_id == user_id, ProcessFileJob.status == JobStatus.completed).join(UploadConfiguration, UploadConfiguration.id == ProcessFileJob.config_id).filter(UploadConfiguration.transaction_source_id == transaction_source_id)

    for job in query:
        job.kind = JobKind.recategorize
        job.attempt_count = 0
        job.status = JobStatus.pending
        session.add(job)

    session.commit()

def enqueue_or_reset_job(
    session: Session,
    user_id: int,
    pdf_id: int,
    job_kind: JobKind,
) -> ProcessFileJob:

    existing_job = session.query(ProcessFileJob).filter(ProcessFileJob.pdf_id == pdf_id).one_or_none()

    job: ProcessFileJob
    if existing_job:
        existing_job.status = JobStatus.pending
        existing_job.kind = job_kind
        existing_job.attempt_count=0
        session.add(existing_job)
        session.commit()
        job = existing_job

    else:
        new_job = ProcessFileJob(
            created_at=datetime.now(timezone.utc),
            last_tried_at=None,
            status=JobStatus.pending,  
            user_id=user_id,
            config_id=None,
            job_kind=job_kind,
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
    pdf_ids:list[int],
    job_kind: JobKind,
) -> list[ProcessFileJob]:

    out = []
    for pdf_id in pdf_ids:
        out.append(enqueue_or_reset_job(session, user_id, pdf_id, job_kind=job_kind))
    return out
    

