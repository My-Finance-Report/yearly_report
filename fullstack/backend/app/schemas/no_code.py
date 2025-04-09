import enum
from typing import Generic, TypeVar
from pydantic import BaseModel
from pydantic.generics import GenericModel
from abc import ABC, abstractmethod
from app.core.db import Session
from app.models import User

from typing import Union, List, Generic, TypeVar

from app.func_utils import pipe
from dataclasses import dataclass

T = TypeVar("T")

@dataclass
class PipelineStart:
    user: User
    session: Session


class Primitive(GenericModel, Generic[T]):
    name: str
    value: T

class OutputType(str, enum.Enum):
    show_value = "show_value"
    show_list = "show_list"
    

@dataclass
class PipelineEnd:
    result: Primitive
    output_type: OutputType



V = TypeVar("V")


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

    @property
    @abstractmethod
    def output_type(self):
        ...

    @abstractmethod
    def call(self, start: PipelineStart) -> Union[Primitive[T], List[Primitive[T]]]:
        pass


class Output(ABC, Generic[T]):

    @property
    @abstractmethod
    def input_type(self) -> type[T]:
        ...

    @abstractmethod
    def call(self, data: T) -> None:
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


def evaluate_pipeline(steps: List[Transformation | Generator], session: Session, user: User)-> PipelineEnd:

    args: PipelineStart = PipelineStart(user, session)
    curr_args: Primitive = steps[0].call(args)
    for step in steps[1:-1]:
        curr_args = step.call(curr_args)
    return steps[-1].call(curr_args)
    
        
    