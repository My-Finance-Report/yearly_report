from collections.abc import Callable
from dataclasses import asdict, dataclass, is_dataclass
from decimal import Decimal
from functools import partial
from typing import Any, TypeVar, get_args, get_origin


from app.db import Session
from app.models import User
from app.no_code.generators import account_balance, account_name, first_n_transactions
from app.no_code.outputs import show_list, show_value
from app.no_code.transformations import average_transform, sum_transform
from app.schemas.no_code import (
    NoCodeTool,
    NoCodeToolIn,
    Parameter,
    ParameterType,
    PipelineEnd,
    PipelineStart,
    ResultType,
    ToolType,
)

T = TypeVar("T")



tool_type_map: dict = {  # type: ignore
    ToolType.first_ten_transactions: first_n_transactions,
    ToolType.sum: sum_transform,
    ToolType.average: average_transform,
    ToolType.show_value: show_value,
    ToolType.show_list: show_list,
    ToolType.account_name: account_name,
    ToolType.account_balance: account_balance,
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


def convert_to_pipeline(tools: list[NoCodeToolIn]) -> list:
    steps = []
    for tool in tools:
        func = tool_type_map[tool.tool]
        if tool.parameters:
            kwargs = {p.name: p.value for p in tool.parameters}
            steps.append(partial(func, kwargs=kwargs))
        else:
            steps.append(partial(func, kwargs={}))

    return steps


def serialize_to_result(obj) -> ResultType:
    if isinstance(obj, (Decimal, str, int, float)):
        return obj
    elif is_dataclass(obj):
        # Convert dataclass to dict and serialize each value
        return {
            k: serialize_to_result(v)
            for k, v in asdict(obj).items()
        }
    elif isinstance(obj, dict):
        return {k: serialize_to_result(v) for k, v in obj.items()}
    elif isinstance(obj, (list, tuple)):
        return [serialize_to_result(item) for item in obj]
    else:
        # Handle other types like UUID, datetime etc.
        return str(obj)


def evaluate_pipeline(steps: list[partial[Any]], session: Session, user: User) -> ResultType:
    data = PipelineStart(user, session)
    for block in steps:
        data = block(data)
    return serialize_to_result(data)


def _same(t1: type, t2: type) -> bool:
    if t1 == t2:
        return True
    o1, o2 = get_origin(t1), get_origin(t2)
    return o1 == o2 and all(
        _same(a, b) for a, b in zip(get_args(t1), get_args(t2), strict=False)
    )
