from typing import  NewType
from app.models.models import Base
from sqlalchemy import (
    ForeignKey,
    Integer,
)
from sqlalchemy.orm import Mapped, mapped_column
from app.models.no_code.tool import ToolId
from app.models.no_code.widget import WidgetId



PipelineStepId = NewType("PipelineStepId", int)


class NoCodePipelineStep(Base):
    __tablename__ = "no_code_pipeline_step"
    id: Mapped[PipelineStepId] = mapped_column(
        Integer, primary_key=True, autoincrement=True
    )
    widget_id: Mapped[WidgetId] = mapped_column(
        ForeignKey("no_code_widget.id"), nullable=False
    )
    tool_id: Mapped[ToolId] = mapped_column(
        ForeignKey("no_code_tool.id"), nullable=False
    )
    order_index: Mapped[int] = mapped_column(Integer, nullable=False)

