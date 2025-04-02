from datetime import datetime, timezone
from typing import NewType

from sqlalchemy import Boolean, Column, DateTime, ForeignKey, Integer, JSON, String
from sqlalchemy.orm import Mapped, mapped_column, relationship

from app.db.base_class import Base

SavedFilterId = NewType("SavedFilterId", int)

class SavedFilter(Base):
    """Model for saved filter configurations."""
    
    __tablename__ = "saved_filter"
    
    id: Mapped[SavedFilterId] = mapped_column(Integer, primary_key=True, autoincrement=True)
    name: Mapped[str] = mapped_column(String, nullable=False)
    description: Mapped[str | None] = mapped_column(String, nullable=True)
    filter_data: Mapped[dict] = mapped_column(JSON, nullable=False)  # Stores the filter configuration
    
    # Link to the user who created this filter
    user_id: Mapped[int] = mapped_column(Integer, ForeignKey("user.id"), nullable=False)
    
    # Filter can be public (shareable) or private
    is_public: Mapped[bool] = mapped_column(Boolean, default=False)
    
    # Timestamps
    created_at: Mapped[datetime] = mapped_column(
        DateTime, default=lambda: datetime.now(timezone.utc)
    )
    updated_at: Mapped[datetime] = mapped_column(
        DateTime, default=lambda: datetime.now(timezone.utc), onupdate=lambda: datetime.now(timezone.utc)
    )
