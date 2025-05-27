import csv
import hashlib
import io
import os
import subprocess
import tempfile
import uuid
from datetime import datetime, timezone

from fastapi import APIRouter, Depends, File, HTTPException, UploadFile

from app.async_pipelines.uploaded_file_pipeline.local_types import PdfParseException
from app.db import (
    Session,
    get_current_user,
    get_db,
)
from app.local_types import ProcessFileJobOut, UploadedPdfOut
from app.models.transaction import Transaction
from app.models.transaction_source import TransactionSourceId
from app.models.upload_configuration import UploadConfiguration, UploadConfigurationId
from app.models.uploaded_pdf import UploadedPdf, UploadedPdfId
from app.models.user import User
from app.models.worker_job import JobKind, JobStatus, WorkerJob
from app.models.worker_status import ProcessingState

from app.worker.enqueue_job import enqueue_or_reset_job
from app.worker.status import update_worker_status

router = APIRouter(prefix="/uploads", tags=["uploads"])


def extract_text_from_csv(content_bytes: bytes) -> str:
    text_stream = io.StringIO(content_bytes.decode("utf-8", errors="replace"))
    reader = csv.reader(text_stream)
    lines = [", ".join(row) for row in reader]
    return "\n".join(lines)


def extract_text_from_pdf(content_bytes: bytes) -> str:
    """Extract text from a PDF file using `pdftotext`."""
    with tempfile.NamedTemporaryFile(suffix=".pdf", delete=False) as tmp:
        tmp.write(content_bytes)
        tmp_path = tmp.name

        command = "pdftotext"
        args = ["-layout", tmp_path, "-"]

    try:
        result = subprocess.run(
            [command] + args,
            capture_output=True,
            check=True,
            text=True,
        )
        return result.stdout.strip()
    except subprocess.CalledProcessError as e:
        raise PdfParseException(f"Failed to extract text from PDF: {e.stderr}") from e

    finally:
        if os.path.exists(tmp_path):
            os.remove(tmp_path)


def get_raw_text(content_bytes: bytes, filename: str) -> str:
    _, extension = os.path.splitext(filename.lower())
    if extension == ".pdf":
        return extract_text_from_pdf(content_bytes)
    elif extension == ".csv":
        return extract_text_from_csv(content_bytes)
    else:
        raise ValueError("Unsupported file type")


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
        extracted_text = get_raw_text(content_bytes, file.filename or "")

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
        session=session,
        user_id=user.id,
        pdf_id=uploaded_file.id,
        job_kind=JobKind.full_upload,
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
        session.query(WorkerJob)
        .filter(WorkerJob.id == job_id, WorkerJob.user_id == user.id)
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
        session.query(WorkerJob)
        .filter(WorkerJob.pdf_id.in_(file_ids), WorkerJob.user_id == user.id)
        .all()
    )
    job_lookup: dict[UploadedPdfId, ProcessFileJobOut] = {
        job.pdf_id: ProcessFileJobOut.model_validate(job)
        for job in jobs
        if job.pdf_id is not None
    }
    transaction_source_lookup: dict[UploadConfigurationId, TransactionSourceId] = {
        config.id: config.transaction_source_id
        for config in session.query(UploadConfiguration)
        .filter(UploadConfiguration.user_id == user.id)
        .all()
    }

    vals = []

    for file in files:
        job = job_lookup.get(file.id)

        if job is None or job.config_id is None:
            continue
        config_id = transaction_source_lookup[UploadConfigurationId(job.config_id)]

        vals.append(
            UploadedPdfOut.model_validate(file).model_copy(
                update={"job": job, "transaction_source_id": config_id}
            )
        )

    return sorted(vals, key=lambda x: x.filename, reverse=True)


@router.post("/", response_model=list[UploadedPdfOut])
def upload_files(
    files: list[UploadFile] = File(...),
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> list[UploadedPdfOut]:
    out: list[UploadedPdfOut] = []
    for file in files:
        batch_id = str(uuid.uuid4())
        update_worker_status(
            session,
            user,
            ProcessingState.waiting,
            "waiting for the file to be picked up by a processor",
            batch_id,
        )
        pdf = uploaded_pdf_from_raw_content(session, user, file)
        out.append(pdf)
    return out


@router.get("/is_uploading", response_model=bool)
def is_uploading(
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> bool:
    return bool(
        session.query(WorkerJob)
        .filter(
            WorkerJob.user_id == user.id,
            WorkerJob.status.in_([JobStatus.pending, JobStatus.processing]),
        )
        .all()
    )


@router.delete("/{file_id}", response_model=None)
def delete_file(
    file_id: int,
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> None:
    file = (
        session.query(UploadedPdf)
        .filter(UploadedPdf.id == file_id, UploadedPdf.user_id == user.id)
        .one()
    )

    session.query(Transaction).filter(
        Transaction.uploaded_pdf_id == file.id, Transaction.user_id == user.id
    ).delete()
    session.query(WorkerJob).filter(
        WorkerJob.pdf_id == file.id, WorkerJob.user_id == user.id
    ).delete()
    session.delete(file)

    session.commit()
    return None
