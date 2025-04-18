from dataclasses import asdict, is_dataclass
from decimal import Decimal
from functools import partial
from typing import Any, TypeVar, get_args, get_origin

from pydantic import BaseModel, Json

from app.db import Session
from app.models import TransactionSource, User
from app.no_code.decoration import PipelineCallable, get_tool_callable
from app.schemas.no_code import (
    NoCodeToolIn,
    Parameter,
    PipelineStart,
    SelectOption,
)

T = TypeVar("T")


def make_account_choices(session: Session, user: User) -> list[SelectOption]:
    accts = (
        session.query(TransactionSource.name, TransactionSource.id)
        .filter(TransactionSource.user_id == user.id, ~TransactionSource.archived)
        .all()
    )
    return [SelectOption(key=acct.id, value=acct.name) for acct in accts]


def init_no_code() -> None:
    import app.no_code.generators
    import app.no_code.transformations


def figure_out_parameters(
    parameter: Parameter,
) -> (
    str
    | int
    | float
    | SelectOption
    | list[str]
    | list[SelectOption]
    | list[Decimal]
    | None
):
    if parameter.value is not None:
        return parameter.value

    if parameter.default_value is not None:
        return parameter.default_value

    if parameter.options:
        return parameter.options[0]

    return None


def convert_to_pipeline(
    tools: list[NoCodeToolIn],
) -> list[partial[PipelineCallable]] | None:
    steps = []
    for tool in tools:
        func = get_tool_callable(tool.tool)
        if tool.parameters:
            if not all(
                figure_out_parameters(p) for p in tool.parameters
            ):  # has bug doesnt allow None as param type
                return None
            else:
                kwargs = {p.name: figure_out_parameters(p) for p in tool.parameters}
                steps.append(partial(func, **kwargs))
        else:
            steps.append(partial(func, **{}))

    return steps


def generate_runtime_parameters(tools: list[NoCodeToolIn]) -> list[Parameter]:
    runtime_params = []
    for tool in tools:
        if tool.parameters:
            runtime_params.extend([p for p in tool.parameters if p.is_runtime])
    return runtime_params


def enrich_with_runtime(
    tools: list[NoCodeToolIn], runtime_parameters: list[Parameter] | None = None
) -> list[NoCodeToolIn]:
    param_value_lookup = (
        {param.name: param for param in runtime_parameters}
        if runtime_parameters
        else {}
    )
    enriched = []
    for tool in tools:
        if tool.parameters:
            for param in tool.parameters:
                if param.is_runtime:
                    the_param = param_value_lookup.get(param.name)
                    if the_param:
                        param.value = the_param.value

        enriched.append(tool)
    return enriched


def serialize_to_result(obj: Any) -> Any:
    if isinstance(obj, Decimal | str | int | float):
        return obj
    elif isinstance(obj, BaseModel):
        return obj.model_dump()
    elif is_dataclass(obj):
        if not isinstance(obj, type):
            return {k: serialize_to_result(v) for k, v in asdict(obj).items()}
    elif isinstance(obj, dict):
        return {k: serialize_to_result(v) for k, v in obj.items()}
    elif isinstance(obj, list | tuple):
        return [serialize_to_result(item) for item in obj]
    else:
        return str(obj)


def evaluate_pipeline(
    steps: list[partial[PipelineCallable]] | None, session: Session, user: User
) -> Any | None:
    if not steps:
        return None

    data = PipelineStart(user, session)
    for block in steps:
        data = block(data)  # type: ignore [assignment]
    return serialize_to_result(data)


def _same(t1: type, t2: type) -> bool:
    if t1 == t2:
        return True
    o1, o2 = get_origin(t1), get_origin(t2)
    return o1 == o2 and all(
        _same(a, b) for a, b in zip(get_args(t1), get_args(t2), strict=False)
    )
