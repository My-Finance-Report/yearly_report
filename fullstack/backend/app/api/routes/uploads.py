import hashlib
import os
import subprocess
import tempfile
from fastapi import APIRouter, Depends, UploadFile, File, HTTPException
from datetime import datetime, timezone


from app.api.deps import (
    get_current_user,
)
from app.db import Session, get_db
from app.models import (
    UploadedPdf,
    User,
)

from app.local_types import (
    UploadedPdfOut
)

router = APIRouter(prefix="/uploads", tags=["uploads"])


class PdfParseException(Exception):
    pass

def extract_text_from_pdf(pdf_path: str) -> str:
    command = "pdftotext"
    args = ["-layout", pdf_path, "-"]
    try:
        result = subprocess.run(
            [command] + args,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            check=True,
            text=True  
        )
        return result.stdout
    except subprocess.CalledProcessError as e:
        raise PdfParseException(f"Failed to extract text from PDF: {e.stderr}") from e

def generate_from_raw_content(session,user,filename,content_bytes,raw_hash)->UploadedPdf:
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
    

    new_pdf = UploadedPdf(
        filename=filename,
        raw_content=extracted_text,
        raw_content_hash=raw_hash,
        upload_time=datetime.now(timezone.utc),
        user_id=user.id,
        archived=False,
    )
    session.add(new_pdf)
    session.commit()
    session.refresh(new_pdf)

    return UploadedPdfOut.from_orm(new_pdf)


@router.get(
    "/",
    dependencies=[Depends(get_current_user)],
    response_model=list[UploadedPdfOut],
)
def get_uploads(session:Session =Depends(get_db), user: User = Depends(get_current_user)) -> list[UploadedPdf  ]:
    val =  session.query(UploadedPdf).filter(UploadedPdf.user_id == user.id).all()
    return val
 
  
@router.post("/", response_model=list[UploadedPdfOut])
def upload_files(
    files: list[UploadFile] = File(...),
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
)-> list[UploadedPdf]:
    """
    Upload one or more files. For each file, check if a file with the same
    content hash already exists. If it does, return that; otherwise, generate
    a new UploadedPdf record.
    """
    results:list[UploadedPdf] = []
    for file in files:
        content_bytes = file.file.read()
        raw_hash = hashlib.md5(content_bytes).hexdigest()

        existing_file = (
            session.query(UploadedPdf)
            .filter(UploadedPdf.user_id == user.id, UploadedPdf.raw_content_hash == raw_hash)
            .one_or_none()
        )

        if existing_file:
            results.append(existing_file)
        else:
            new_pdf = generate_from_raw_content(session, user, file.filename, content_bytes, raw_hash)
            results.append(new_pdf)
    return results