from datetime import datetime, timezone

from app.models import JobStatus, ProcessFileJob, UploadedPdf
from sqlalchemy.orm import Session


def enqueue_or_reset_job(
    session: Session,
    user_id: int,
    pdf_id: int,
) -> ProcessFileJob:
    print("in the enqueue func")

    existing_job = session.query(ProcessFileJob).filter(ProcessFileJob.pdf_id == pdf_id).one_or_none()
    job: ProcessFileJob


    if existing_job:
        existing_job.status = JobStatus.pending
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
) -> list[ProcessFileJob]:

    out = []
    for pdf_id in pdf_ids:
        out.append(enqueue_or_reset_job(session, user_id, pdf_id))
    return out
    