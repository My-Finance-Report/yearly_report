from typing import NewType
from app.models.models import Base
from sqlalchemy import (
    ForeignKey,
    UniqueConstraint,
    Boolean,
    Text,
    Integer,
)
from sqlalchemy.orm import Mapped, mapped_column

from app.models.user import UserId


ColChartConfigId = NewType("ColChartConfigId", int)


class ColChartConfig(Base):
    __tablename__ = "col_chart_config"

    id: Mapped[ColChartConfigId] = mapped_column(
        Integer, primary_key=True, autoincrement=True
    )
    name: Mapped[str] = mapped_column(Text, nullable=False)
    user_id: Mapped[UserId] = mapped_column(ForeignKey("user.id"), nullable=False)
    active: Mapped[bool] = mapped_column(Boolean, nullable=False)

    __table_args__ = (
        UniqueConstraint("user_id", "active", name="uq_col_chart_config"),
    )
