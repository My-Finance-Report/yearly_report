import enum
from typing import NewType

from pydantic import BaseModel
from app.models.models import Base, JSONType
from sqlalchemy import (
    JSON,
    Boolean,
    ForeignKey,
    Enum,
    String,
    Integer,
)
from sqlalchemy.orm import Mapped, mapped_column
from app.models.no_code.canvas import CanvasId
from app.models.no_code.widget import WidgetId

from app.models.user import UserId


class ParameterType(str, enum.Enum):
    INT = "int"
    FLOAT = "float"
    STRING = "string"
    SELECT = "select"
    SUBMIT = "submit"
    DATETIME = "datetime"
    PAGINATION = "pagination"
    MULTI_SELECT = "multi_select"


class ParameterOptionSourceType(str, enum.Enum):
    STATIC = "static"
    DYNAMIC = "dynamic"


class SelectOption(BaseModel):
    key: str
    value: str


class DefaultValue(BaseModel):
    value: SelectOption | list[SelectOption] | None | float | bool


class DisplaySize(str, enum.Enum):
    SMALL = "small"
    MEDIUM = "medium"
    LARGE = "large"


class DisplayInfo(BaseModel):
    views: list[str]
    size: DisplaySize | None = None
    show_label: bool = True
    row: int
    col: int
    row_span: int
    col_span: int


ParameterId = NewType("ParameterId", int)
ParameterGroupId = NewType("ParameterGroupId", int)


class NoCodeParameter(Base):
    __tablename__ = "no_code_parameter"

    id: Mapped[ParameterId] = mapped_column(
        Integer, primary_key=True, autoincrement=True
    )
    type: Mapped[ParameterType] = mapped_column(Enum(ParameterType), nullable=False)
    user_id: Mapped[UserId] = mapped_column(ForeignKey("user.id"), nullable=False)
    name: Mapped[str] = mapped_column(String, nullable=False)
    label: Mapped[str] = mapped_column(String, nullable=False)
    group_id: Mapped[ParameterGroupId] = mapped_column(
        ForeignKey("no_code_parameter_group.id"), nullable=False
    )
    trigger_refetch: Mapped[bool] = mapped_column(Boolean, nullable=False, default=True)
    dependent_widgets: Mapped[list[WidgetId]] = mapped_column(JSON, default=list)
    option_source_type: Mapped[ParameterOptionSourceType] = mapped_column(
        Enum(ParameterOptionSourceType), nullable=True
    )
    option_generator_key: Mapped[str] = mapped_column(String, nullable=True)
    default_value: Mapped[DefaultValue] = mapped_column(
        JSONType(DefaultValue), nullable=True
    )
    display_info: Mapped[DisplayInfo] = mapped_column(
        JSONType(DisplayInfo), nullable=True
    )


class NoCodeParameterOption(Base):
    __tablename__ = "no_code_parameter_option"

    id: Mapped[int] = mapped_column(Integer, primary_key=True, autoincrement=True)
    parameter_id: Mapped[ParameterId] = mapped_column(
        ForeignKey("no_code_parameter.id"), nullable=False
    )
    key: Mapped[str] = mapped_column(String, nullable=False)
    value: Mapped[str] = mapped_column(String, nullable=False)


class ParameterGroupType(str, enum.Enum):
    GLOBAL = "global"
    WIDGET = "widget"


class NoCodeParameterGroup(Base):
    __tablename__ = "no_code_parameter_group"

    id: Mapped[ParameterGroupId] = mapped_column(
        Integer, primary_key=True, autoincrement=True
    )
    user_id: Mapped[UserId] = mapped_column(ForeignKey("user.id"), nullable=False)
    group_type: Mapped[ParameterGroupType] = mapped_column(
        Enum(ParameterGroupType), nullable=False
    )
    name: Mapped[str] = mapped_column(String, nullable=False)
    canvas_id: Mapped[CanvasId] = mapped_column(
        ForeignKey("no_code_canvas.id"), nullable=False
    )
