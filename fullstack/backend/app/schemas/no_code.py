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

    @property
    @abstractmethod
    def input_type(self):
        ...

    @property
    @abstractmethod
    def output_type(self):
        ...

    @abstractmethod
    def transform(self, data: FullPrimitive[ValueType]) -> FullPrimitive[ValueType]:
        """
        Consumes one or more Primitives of type NumericType
        and returns a single Primitive of the same type (or possibly a new type).
        """
        pass

class Generator(ABC, Generic[ValueType]):

    @property
    @abstractmethod
    def output_type(self):
        ...

    @abstractmethod
    def generate(self) -> FullPrimitive[ValueType]:
        pass


class Output(ABC, Generic[ValueType]):

    @property
    @abstractmethod
    def input_type(self):
        ...

    @abstractmethod
    def produce(self, data: FullPrimitive[ValueType]) -> None:
        pass


def lint_pipeline(steps: List[ABC]) -> None:
    """
    Checks that each step's input_type matches the previous step's output_type.
    Raises a ValueError if there's a mismatch.
    """
    if not steps:
        raise ValueError("No steps in the pipeline.")

    first = steps[0]
    if not hasattr(first, "output_type"):
        raise ValueError("First step must have an 'output_type' (e.g., a Generator).")

    current_type = getattr(first, "output_type", None)

    for i in range(1, len(steps)):
        step = steps[i]
        expected_input = getattr(step, "input_type", None)
        if expected_input is None:
            raise ValueError(f"Step {i} ({step.__class__.__name__}) has no 'input_type' declared.")

        if current_type != expected_input:
            raise ValueError(
                f"Type mismatch at step {i} ({step.__class__.__name__}): "
                f"expected {expected_input}, got {current_type}."
            )

        if hasattr(step, "output_type"):
            current_type = getattr(step, "output_type", None)

