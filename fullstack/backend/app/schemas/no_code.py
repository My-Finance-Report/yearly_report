from datetime import datetime
import enum
from dataclasses import dataclass
from decimal import Decimal
from typing import Any, Generic, TypeVar

from pydantic import BaseModel
from sqlalchemy.orm import Session

from app.models import TransactionKind, User


class NoCodeTransaction(BaseModel):
    amount: float
    description: str
    date_of_transaction: datetime
    kind: TransactionKind
    category_name: str


T = TypeVar("T")
V = TypeVar("V")


class ParameterType(str, enum.Enum):
    INT = "int"
    FLOAT = "float"
    STRING = "string"
    SELECT = "select"
    SUBMIT ="submit"
    DATETIME = "datetime"
    PAGINATION = "pagination"
    MULTI_SELECT = "multi_select"


class SelectOption(BaseModel):
    key: str
    value: str


class DisplaySize(str, enum.Enum):
    SMALL = "small"
    MEDIUM = "medium"
    LARGE = "large"


class DisplayInfo(BaseModel):
    size: DisplaySize | None = None
    row: int
    col: int
    row_span: int
    col_span: int


class Parameter(BaseModel):
    name: str
    label: str | None = None
    type: ParameterType
    value: (
        int
        | float
        | str
        | SelectOption
        | list[str]
        | list[Decimal]
        | list[SelectOption]
        | None
    ) = None
    default_value: (
        int
        | float
        | str
        | SelectOption
        | list[str]
        | list[Decimal]
        | list[SelectOption]
        | None
    ) = None
    options: list[SelectOption] | None = None
    option_generator: str | None = None
    widget_id: str | None = None
    is_runtime: bool = False
    display_info: DisplayInfo | None = None


@dataclass
class PipelineStart:
    user: User
    session: Session


class Primitive(BaseModel, Generic[T]):
    name: str
    value: T


Blah = Decimal | NoCodeTransaction
ValidPassable = Decimal | list[NoCodeTransaction]

PrimitiveResultValue = Blah | list[Decimal] | list[NoCodeTransaction]


class PrimitiveResult(BaseModel):
    name: str
    value: PrimitiveResultValue


class OutputType(str, enum.Enum):
    show_value = "show_value"
    show_list = "show_list"


@dataclass
class PipelineEnd:
    result: PrimitiveResult
    output_type: OutputType


class WidgetType(str, enum.Enum):
    value = "value"
    value_with_trend = "value_with_trend"
    badge = "badge"
    list = "list"
    pie_chart = "pie_chart"
    bar_chart = "bar_chart"
    separator = "separator"
    form="form"


Scalar = Decimal | str | int | float
Object = dict[str, Scalar]
ResultType = Scalar | Object | list[Scalar] | list[Object] | dict[Scalar, Object]


class ResultTypeEnum(str, enum.Enum):
    string = "string"
    number = "number"
    object_ = "object"
    list_ = "list"
    deferred = "deferred"


class NoCodeToolIn(BaseModel):
    tool: str
    parameters: list[Parameter] | None = None


class NoCodeWidgetIn(BaseModel):
    name: str
    description: str
    pipeline: list[NoCodeToolIn]
    row: int
    col: int
    row_span: int
    col_span: int
    type: WidgetType


class NoCodeWidgetOut(BaseModel):
    id: str
    name: str
    description: str
    result: Any
    result_type: ResultTypeEnum
    parameters: list[Parameter]
    row: int
    col: int
    row_span: int
    col_span: int
    type: WidgetType


class NoCodeCanvas(BaseModel):
    name: str
    widgets: list[NoCodeWidgetOut]
    parameters: list[Parameter]


class NoCodeTool(BaseModel):
    name: str
    description: str
    tool: str
    parameters: list[Parameter] | None = None
    return_type: dict[str, Any]
    input_type: dict[str, Any]
