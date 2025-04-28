from datetime import datetime
from typing import NewType
from app.models.models import Base
from sqlalchemy import (
    Boolean,
    DateTime,
    ForeignKey,
    UniqueConstraint,
    Text,
    Integer,
)
from sqlalchemy.orm import Mapped, mapped_column
from sqlalchemy import DateTime
from datetime import datetime

from app.models.user import UserId


UploadedPdfId = NewType("UploadedPdfId", int)


class UploadedPdf(Base):
    __tablename__ = "uploaded_pdf"

    id: Mapped[UploadedPdfId] = mapped_column(
        Integer, primary_key=True, autoincrement=True
    )
    filename: Mapped[str] = mapped_column(Text, nullable=False)
    nickname: Mapped[str | None] = mapped_column(Text, nullable=True)
    raw_content: Mapped[str] = mapped_column(Text, nullable=False)
    raw_content_hash: Mapped[str] = mapped_column(Text, nullable=False)
    upload_time: Mapped[datetime] = mapped_column(DateTime, nullable=False)
    user_id: Mapped[UserId] = mapped_column(ForeignKey("user.id"), nullable=False)
    archived: Mapped[bool] = mapped_column(Boolean, default=False)

    __table_args__ = (
        UniqueConstraint("raw_content_hash", "user_id", name="uq_uploaded_pdf"),
    )
