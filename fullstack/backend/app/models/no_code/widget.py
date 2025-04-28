import enum
from typing import NewType
from app.models.models import Base
from sqlalchemy import (
    ForeignKey,
    String,
    Enum,
    Integer,
)
from sqlalchemy.orm import Mapped, mapped_column
from app.models.no_code.canvas import CanvasId

from app.models.user import UserId


WidgetId = NewType("WidgetId", int)


class WidgetType(str, enum.Enum):
    value = "value"
    value_with_trend = "value_with_trend"
    badge = "badge"
    list = "list"
    pie_chart = "pie_chart"
    bar_chart = "bar_chart"
    separator = "separator"
    form = "form"


class NoCodeWidget(Base):
    __tablename__ = "no_code_widget"

    id: Mapped[WidgetId] = mapped_column(Integer, primary_key=True, autoincrement=True)
    user_id: Mapped[UserId] = mapped_column(ForeignKey("user.id"), nullable=False)
    name: Mapped[str] = mapped_column(String, nullable=False)
    widget_type: Mapped[WidgetType] = mapped_column(Enum(WidgetType), nullable=False)
    label: Mapped[str] = mapped_column(String, nullable=True)
    canvas_id: Mapped[CanvasId] = mapped_column(
        ForeignKey("no_code_canvas.id"), nullable=False
    )
    row: Mapped[int] = mapped_column(Integer, nullable=False)
    column: Mapped[int] = mapped_column(Integer, nullable=False)
    row_span: Mapped[int] = mapped_column(Integer, nullable=False)
    col_span: Mapped[int] = mapped_column(Integer, nullable=False)
