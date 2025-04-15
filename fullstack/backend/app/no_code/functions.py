import enum
from collections.abc import Callable
from dataclasses import dataclass
from functools import partial
from typing import Any, TypeVar, get_args, get_origin

from pydantic import BaseModel

from app.db import Session
from app.models import User
from app.no_code.generators import first_n_transactions
from app.no_code.outputs import show_list, show_value
from app.no_code.transformations import average_transform, sum_transform
from app.schemas.no_code import (
    Parameter,
    ParameterType,
    PipelineEnd,
    PipelineStart,
)

T = TypeVar("T")


class ToolType(str, enum.Enum):
    first_ten_transactions = "first_ten_transactions"
    sum = "sum"
    average = "average"
    show_value = "show_value"
    show_list = "show_list"


class NoCodeTool(BaseModel):
    name: str
    description: str
    tool: ToolType
    parameters: list[Parameter] | None = None

class NoCodeToolIn(BaseModel):
    tool: ToolType
    parameters: list[Parameter] | None = None


tool_type_map: dict = {  # type: ignore
    ToolType.first_ten_transactions: first_n_transactions,
    ToolType.sum: sum_transform,
    ToolType.average: average_transform,
    ToolType.show_value: show_value,
    ToolType.show_list: show_list,
}


def make_tools() -> list[NoCodeTool]:
    return [
        NoCodeTool(
            name="First n Transactions",
            description="Get the first n transactions for the user",
            tool=ToolType.first_ten_transactions,
            parameters=[Parameter(name="n", type=ParameterType.INT)],
        ),
        NoCodeTool(
            name="Sum",
            description="Sum of the passed values",
            tool=ToolType.sum,
        ),
        NoCodeTool(
            name="Average",
            description="Average of the passed values",
            tool=ToolType.average,
        ),
        NoCodeTool(
            name="Show Value",
            description="Show the passed value",
            tool=ToolType.show_value,
        ),
        NoCodeTool(
            name="Show List",
            description="Show the passed list",
            tool=ToolType.show_list,
        ),
    ]


@dataclass
class Pipeline:
    first_step: Callable[[PipelineStart], T]
    steps: list[partial[Any]]
    last_step: Callable[[T], PipelineEnd]


def convert_to_pipeline(tools: list[NoCodeToolIn]) -> Pipeline:
    steps = []
    for tool in tools:
        func = tool_type_map[tool.tool]
        if tool.parameters:
            kwargs = {p.name: p.value for p in tool.parameters}
            steps.append(partial(func, kwargs=kwargs))
        else:
            steps.append(partial(func, kwargs={}))

    return Pipeline(steps[0], steps[1:-1], steps[-1])


def evaluate_pipeline(pipeline: Pipeline, session: Session, user: User) -> PipelineEnd:
    data = PipelineStart(user, session)
    data = pipeline.first_step(data)
    for block in pipeline.steps:
        data = block(data)
    val = pipeline.last_step(data)
    return val


def _same(t1: type, t2: type) -> bool:
    if t1 == t2:
        return True
    o1, o2 = get_origin(t1), get_origin(t2)
    return o1 == o2 and all(
        _same(a, b) for a, b in zip(get_args(t1), get_args(t2), strict=False)
    )
