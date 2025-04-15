import enum
from dataclasses import dataclass
from decimal import Decimal
from typing import Generic, TypeVar

from pydantic import BaseModel

from app.core.db import Session
from app.models import TransactionId, User


@dataclass
class NoCodeTransaction:
    id: TransactionId | None
    amount: float
    description: str
    date: str


T = TypeVar("T")
V = TypeVar("V")


class ParameterType(str, enum.Enum):
    INT = "int"
    FLOAT = "float"
    STRING = "string"


class Parameter(BaseModel):
    name: str
    type: ParameterType
    value: int | float | str | None = None


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
    list = "list"
    chart = "chart"

Scalar = Decimal | str | int | float
Object = dict[str, Scalar]
ResultType = Scalar | Object | list[Scalar] | list[Object] 

class ToolType(str, enum.Enum):
    first_ten_transactions = "first_ten_transactions"
    account_name = "account_name"
    account_balance = "account_balance"
    sum = "sum"
    average = "average"
    show_value = "show_value"
    show_list = "show_list"


class ResultTypeEnum(enum.Enum):
    string = "string"
    number = "number"
    transactions = { "id": "number", "amount": "number", "description": "string"}


class NoCodeToolIn(BaseModel):
    tool: ToolType
    parameters: list[Parameter] | None = None



class NoCodeWidget(BaseModel):
    name: str
    description: str
    pipeline: list[NoCodeToolIn]
    result: ResultType
    result_type: ResultTypeEnum
    row: int
    col: int
    height: int
    width: int
    type: WidgetType 


class NoCodeTool(BaseModel):
    name: str
    description: str
    tool: ToolType
    parameters: list[Parameter] | None = None


