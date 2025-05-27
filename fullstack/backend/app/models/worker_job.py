from datetime import datetime
import enum
from typing import NewType
from app.models.models import Base
from sqlalchemy import (
    Boolean,
    Enum,
    DateTime,
    ForeignKey,
    UniqueConstraint,
    Text,
    Integer,
)
from sqlalchemy.orm import Mapped, mapped_column
from sqlalchemy import DateTime
from datetime import datetime, timezone
from app.models.upload_configuration import UploadConfigurationId
from app.models.uploaded_pdf import UploadedPdfId

from app.models.user import UserId


class JobStatus(str, enum.Enum):
    completed = "completed"
    pending = "pending"
    processing = "processing"
    failed = "failed"


class JobKind(str, enum.Enum):
    full_upload = "full_upload"
    recategorize = "recategorize"
    plaid_recategorize = "plaid_recategorize"


WorkerJobId = NewType("WorkerJobId", int)


class WorkerJob(Base):
    __tablename__ = "process_file_job"

    id: Mapped[WorkerJobId] = mapped_column(
        Integer, primary_key=True, autoincrement=True
    )
    created_at: Mapped[datetime] = mapped_column(
        DateTime, default=lambda: datetime.now(timezone.utc)
    )
    last_tried_at: Mapped[datetime | None] = mapped_column(DateTime, nullable=True)
    status: Mapped[JobStatus] = mapped_column(Enum(JobStatus), nullable=False)
    user_id: Mapped[UserId] = mapped_column(ForeignKey("user.id"), nullable=False)
    config_id: Mapped[UploadConfigurationId | None] = mapped_column(
        ForeignKey("upload_configuration.id"), nullable=True
    )
    pdf_id: Mapped[UploadedPdfId | None] = mapped_column(
        ForeignKey("uploaded_pdf.id"), nullable=True
    )
    archived: Mapped[bool] = mapped_column(Boolean, default=False)
    attempt_count: Mapped[int] = mapped_column(Integer, default=0)
    error_messages: Mapped[str] = mapped_column(Text, nullable=True)
    kind: Mapped[JobKind] = mapped_column(Enum(JobKind), nullable=False)

    __table_args__ = (UniqueConstraint("pdf_id", name="uq_process_file_job"),)
