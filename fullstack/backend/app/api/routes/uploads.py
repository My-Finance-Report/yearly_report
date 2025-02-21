import hashlib
import os
import tempfile
from datetime import datetime, timezone

from fastapi import APIRouter, Depends, File, HTTPException, UploadFile

from app.api.deps import (
    get_current_user,
)
from app.async_pipelines.uploaded_file_pipeline.local_types import PdfParseException
from app.async_pipelines.uploaded_file_pipeline.transaction_parser import (
    extract_text_from_pdf,
)
from app.db import Session, get_db
from app.local_types import ProcessFileJobOut, UploadedPdfOut
from app.models import (
    JobKind,
    ProcessFileJob,
    UploadedPdf,
    User,
)
from app.worker.enqueue_job import enqueue_or_reset_job

router = APIRouter(prefix="/uploads", tags=["uploads"])


def uploaded_pdf_from_raw_content(
    session: Session, user: User, file: UploadFile
) -> UploadedPdfOut:
    content_bytes = file.file.read()
    raw_hash = hashlib.md5(content_bytes).hexdigest()

    existing_file = (
        session.query(UploadedPdf)
        .filter(
            UploadedPdf.user_id == user.id, UploadedPdf.raw_content_hash == raw_hash
        )
        .one_or_none()
    )
    uploaded_file: UploadedPdf
    if existing_file:
        uploaded_file = existing_file
    else:
        with tempfile.NamedTemporaryFile(suffix=".pdf", delete=False) as tmp:
            tmp.write(content_bytes)
            tmp_path = tmp.name

        try:
            extracted_text = extract_text_from_pdf(tmp_path)
        except PdfParseException as e:
            os.remove(tmp_path)
            raise HTTPException(status_code=400, detail=str(e))
        finally:
            if os.path.exists(tmp_path):
                os.remove(tmp_path)

        new_file = UploadedPdf(
            filename=file.filename,
            raw_content=extracted_text,
            raw_content_hash=raw_hash,
            upload_time=datetime.now(timezone.utc),
            user_id=user.id,
            archived=False,
        )
        session.add(new_file)
        session.commit()
        session.refresh(new_file)
        uploaded_file = new_file

    print("enqueue the job")
    job = enqueue_or_reset_job(
        session, user.id, uploaded_file.id, job_kind=JobKind.full_upload
    )

    return UploadedPdfOut.model_validate(uploaded_file).model_copy(
        update={"job": ProcessFileJobOut.model_validate(job)}
    )


@router.post("/reprocess/{job_id}", response_model=ProcessFileJobOut)
def reprocess_file(
    job_id: int,
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> ProcessFileJobOut:
    """Reprocess an uploaded file by job ID."""
    job = (
        session.query(ProcessFileJob)
        .filter(ProcessFileJob.id == job_id, ProcessFileJob.user_id == user.id)
        .one_or_none()
    )

    if not job:
        raise HTTPException(status_code=404, detail="Job not found")

    job = enqueue_or_reset_job(
        session, user.id, job.pdf_id, job_kind=JobKind.recategorize
    )

    return ProcessFileJobOut.model_validate(job)


@router.get(
    "/",
    dependencies=[Depends(get_current_user)],
    response_model=list[UploadedPdfOut],
)
def get_uploads(
    session: Session = Depends(get_db), user: User = Depends(get_current_user)
) -> list[UploadedPdfOut]:
    """Retrieve user uploads along with their associated jobs."""

    files = session.query(UploadedPdf).filter(UploadedPdf.user_id == user.id).all()
    file_ids = [file.id for file in files]

    jobs = (
        session.query(ProcessFileJob).filter(ProcessFileJob.pdf_id.in_(file_ids)).all()
    )
    job_lookup = {job.pdf_id: ProcessFileJobOut.from_orm(job) for job in jobs}

    return [
        UploadedPdfOut.from_orm(file).copy(update={"job": job_lookup.get(file.id)})
        for file in files
    ]


@router.post("/", response_model=list[UploadedPdfOut])
def upload_files(
    files: list[UploadFile] = File(...),
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> list[UploadedPdfOut]:
    out: list[UploadedPdfOut] = []
    for file in files:
        pdf = uploaded_pdf_from_raw_content(session, user, file)
        out.append(pdf)
    return out
