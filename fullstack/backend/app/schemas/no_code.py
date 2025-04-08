from typing import Generic, TypeVar
from pydantic import BaseModel
from pydantic.generics import GenericModel
from abc import ABC, abstractmethod
from typing import Union, List, Generic, TypeVar

ValueType = TypeVar("ValueType", int, float, str)

class PrimitiveMetadata(BaseModel):
    source_type: str
    source_details: dict

class Primitive(GenericModel, Generic[ValueType]):
    name: str
    value: ValueType
    metadata: PrimitiveMetadata

FullPrimitive = Union[Primitive[ValueType], List[Primitive[ValueType]]]

class Transformation(ABC, Generic[ValueType]):
    @abstractmethod
    def transform(self, data: FullPrimitive[ValueType]) -> FullPrimitive[ValueType]:
        """
        Consumes one or more Primitives of type NumericType
        and returns a single Primitive of the same type (or possibly a new type).
        """
        pass

class Generator(ABC, Generic[ValueType]):
    @abstractmethod
    def generate(self) -> FullPrimitive[ValueType]:
        pass


class Output(ABC, Generic[ValueType]):
    @abstractmethod
    def produce(self, data: FullPrimitive[ValueType]) -> None:
        pass
