from datetime import datetime
import enum
from dataclasses import dataclass
from decimal import Decimal
from typing import Any, Generic, TypeVar

from pydantic import BaseModel, Field
from sqlalchemy.orm import Session
from app.models.no_code.parameter import (
    DefaultValue,
    DisplayInfo,
    ParameterGroupId,
    ParameterGroupType,
    ParameterType,
    SelectOption,
)
from app.models.no_code.widget import WidgetId, WidgetType
from app.models.transaction import TransactionKind

from app.models.user import User


class NoCodeTransaction(BaseModel):
    amount: float
    description: str
    account_name: str
    date_of_transaction: datetime
    kind: TransactionKind
    category_name: str


T = TypeVar("T")
V = TypeVar("V")


class ParameterGroupOut(BaseModel):
    id: ParameterGroupId
    type: ParameterGroupType
    name: str
    widget_id: None | str = None


class Parameter(BaseModel):
    id: int
    group_id: ParameterGroupId
    name: str
    label: str | None = None
    type: ParameterType
    value: (
        int
        | float
        | str
        | SelectOption
        | datetime
        | bool
        | list[str]
        | list[Decimal]
        | list[SelectOption]
        | None
    ) = None
    default_value: DefaultValue | None = None
    options: list[SelectOption] | None = None
    option_generator: str | None = None
    trigger_refetch: bool = True
    dependent_widgets: list[WidgetId] = Field(default_factory=list)
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
    id: WidgetId
    name: str
    description: str
    pipeline: list[NoCodeToolIn]
    row: int
    col: int
    row_span: int
    col_span: int
    type: WidgetType


class NoCodeWidgetUpdate(BaseModel):
    name: str
    row: int
    col: int
    row_span: int
    col_span: int


class NoCodeParameterUpdate(BaseModel):
    label: str
    row: int
    col: int
    row_span: int
    col_span: int


class NoCodeWidgetOut(BaseModel):
    id: WidgetId
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


class NoCodeCanvasOut(BaseModel):
    name: str
    widgets: list[NoCodeWidgetIn]
    parameters: list[Parameter]
    parameter_groups: list[ParameterGroupOut]


class NoCodeToolOut(BaseModel):
    name: str
    description: str
    tool: str
    parameters: list[Parameter] | None = None
    return_type: dict[str, Any]
    input_type: dict[str, Any]
