import enum
from typing import Generic, TypeVar
from pydantic import BaseModel
from pydantic.generics import GenericModel
from abc import ABC, abstractmethod
from app.core.db import Session
from app.models import User

from typing import Generic, TypeVar

from dataclasses import dataclass


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


class Primitive(GenericModel, Generic[T]):
    name: str
    value: T

PrimitiveResultValue = list[int] | list[float] | list[str] | list[list[int]] | list[list[float]] | list[list[str]]

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


class Transformation(ABC, Generic[T, V]):

    @property
    @abstractmethod
    def input_type(self) -> type[T]:
        ...

    @property
    @abstractmethod
    def output_type(self) -> type[V]:
        ...

    @abstractmethod
    def call(self, data: T) -> V:
        """
        Consumes one or more Primitives of type NumericType
        and returns a single Primitive of the same type (or possibly a new type).
        """
        pass

class Generator(ABC, Generic[T]):

    def __init__(self, kwargs: dict[str, int]):
        self.kwargs = kwargs

    @property
    @abstractmethod
    def parameters(self) -> list[Parameter]:
        ...

    @property
    @abstractmethod
    def output_type(self):
        ...

    @abstractmethod
    def call(self, start: PipelineStart) -> Primitive[T]:
        pass


class Output(ABC, Generic[T]):

    @property
    @abstractmethod
    def input_type(self) -> type[T]:
        ...

    @abstractmethod
    def call(self, data: T) -> PipelineEnd:
        pass


