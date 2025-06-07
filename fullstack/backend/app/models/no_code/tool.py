from typing import NewType
from app.models.models import Base
from sqlalchemy import (
    ForeignKey,
    Integer,
    String,
    UniqueConstraint,
)
from sqlalchemy.orm import Mapped, mapped_column
from app.models.no_code.canvas import CanvasId
from app.models.no_code.parameter import ParameterId
from app.models.no_code.widget import WidgetId

from app.models.user import UserId


ToolId = NewType("ToolId", int)


class NoCodeTool(Base):
    __tablename__ = "no_code_tool"

    id: Mapped[ToolId] = mapped_column(Integer, primary_key=True, autoincrement=True)
    user_id: Mapped[UserId] = mapped_column(ForeignKey("user.id"), nullable=False)
    widget_id: Mapped[WidgetId] = mapped_column(
        ForeignKey("no_code_widget.id"), nullable=False
    )
    tool_name: Mapped[str] = mapped_column(String, nullable=False)
    canvas_id: Mapped[CanvasId] = mapped_column(
        ForeignKey("no_code_canvas.id"), nullable=False
    )

    __table_args__ = (UniqueConstraint("widget_id", "user_id", "canvas_id"),)


class NoCodeToolParameter(Base):
    __tablename__ = "no_code_tool_parameter"
    tool_id: Mapped[ToolId] = mapped_column(
        ForeignKey("no_code_tool.id"), primary_key=True
    )
    parameter_id: Mapped[ParameterId] = mapped_column(
        ForeignKey("no_code_parameter.id"), primary_key=True
    )
