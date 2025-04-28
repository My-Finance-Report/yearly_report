from typing import  NewType
from app.models.models import Base
from sqlalchemy import (
    ForeignKey,
    UniqueConstraint,
    Text,
    Integer,
)
from sqlalchemy.orm import Mapped, mapped_column
from app.models.transaction_source import TransactionSourceId

from app.models.user import UserId



UploadConfigurationId = NewType("UploadConfigurationId", int)


class UploadConfiguration(Base):
    __tablename__ = "upload_configuration"

    id: Mapped[UploadConfigurationId] = mapped_column(
        Integer, primary_key=True, autoincrement=True
    )
    filename_regex: Mapped[str] = mapped_column(Text, nullable=False)
    start_keyword: Mapped[str | None] = mapped_column(Text, nullable=True)
    end_keyword: Mapped[str | None] = mapped_column(Text, nullable=True)
    transaction_source_id: Mapped[TransactionSourceId] = mapped_column(
        ForeignKey("transaction_source.id"), nullable=False
    )
    user_id: Mapped[UserId] = mapped_column(ForeignKey("user.id"), nullable=False)

    __table_args__ = (
        UniqueConstraint("transaction_source_id", name="uq_upload_configuration"),
    )

