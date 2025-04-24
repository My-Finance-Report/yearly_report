from dataclasses import asdict, is_dataclass
from datetime import datetime
from decimal import Decimal
from functools import partial
from typing import Any, TypeVar, get_args, get_origin

from pydantic import BaseModel
from sqlalchemy import func

from app.db import Session
from app.models import Transaction, TransactionSource, User
from app.no_code.decoration import PipelineCallable, get_tool_callable
from app.schemas.no_code import (
    NoCodeToolIn,
    Parameter,
    PipelineStart,
    ResultTypeEnum,
    SelectOption,
)

T = TypeVar("T")


def make_account_choices(session: Session, user: User) -> list[SelectOption]:
    accts = (
        session.query(TransactionSource.name, TransactionSource.id)
        .filter(TransactionSource.user_id == user.id, ~TransactionSource.archived)
        .all()
    )
    return [SelectOption(key=str(acct.id), value=acct.name) for acct in accts]


def init_no_code() -> None:
    import app.no_code.generators
    import app.no_code.updaters
    import app.no_code.transformations


def safe_parse_int(value: Any) -> int | None:
    try:
        return int(value)
    except (ValueError, TypeError):
        return None


def determine_result_type(result: Any) -> ResultTypeEnum:
    if result is None:
        return ResultTypeEnum.deferred
    if is_dataclass(result):
        return ResultTypeEnum.object_
    if isinstance(result, list):
        return ResultTypeEnum.list_
    if safe_parse_int(result) is not None:
        return ResultTypeEnum.number
    return ResultTypeEnum.string


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
    | bool
    | datetime
    | None
):
    if parameter.value is not None:
        return parameter.value

    if parameter.default_value is not None:
        return parameter.default_value

    if parameter.options:
        return parameter.options[0]

    return None


def convert_to_callable_pipeline(
    tools: list[NoCodeToolIn],
) -> list[partial[PipelineCallable]] | None:
    steps = []
    for tool in tools:
        func = get_tool_callable(tool.tool)
        if tool.parameters:
            kwargs = {p.name: figure_out_parameters(p) for p in tool.parameters}
            steps.append(partial(func, **kwargs))
        else:
            steps.append(partial(func, **{}))

    return steps


def get_pages_per_account(
    session: Session, user: User, **kwargs: Any
) -> list[SelectOption]:
    account_id = kwargs["account_id"]
    n = kwargs["n"]

    total = (
        session.query(func.count(Transaction.id))
        .filter(
            Transaction.user_id == user.id,
            Transaction.transaction_source_id == int(account_id.key),
        )
        .scalar()
    )

    number_of_pages = (total // int(n.key)) + 1

    return [
        SelectOption(key=str(page), value=str(page))
        for page in range(1, number_of_pages + 1)
    ]


CALLABLE_LOOKUP = {"get_pages_per_account": get_pages_per_account}


def extract_parameters_from_pipeline(
    tools: list[NoCodeToolIn], session: Session, user: User
) -> list[Parameter]:
    runtime_params = []
    for tool in tools:
        if tool.parameters:
            for p in tool.parameters:
                runtime_params.append(p)

    lookup = {param.name: figure_out_parameters(param) for param in runtime_params}

    for param in runtime_params:
        if param.option_generator:
            param.options = CALLABLE_LOOKUP[param.option_generator](
                session, user, **lookup
            )

    return runtime_params


def enrich_tools_with_runtime_parameters(
    tools: list[NoCodeToolIn],
    runtime_parameters: list[Parameter] | None = None,
    widget_id: str | None = None,
) -> list[NoCodeToolIn]:
    widget_specific_params = (
        {
            param.name: param
            for param in runtime_parameters
            if param.widget_id is not None and param.widget_id == widget_id
        }
        if runtime_parameters
        else {}
    )

    param_value_lookup = (
        {param.name: param for param in runtime_parameters if param.widget_id is None}
        if runtime_parameters
        else {}
    )
    enriched = []
    for tool in tools:
        if tool.parameters:
            for param in tool.parameters:
                if param.is_runtime:
                    if param.name in widget_specific_params:
                        param.value = widget_specific_params[param.name].value
                    else:
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
    return data


class RenderLoopResult(BaseModel):
    result: Any
    result_type: ResultTypeEnum
    parameters: list[Parameter]


def main_render_loop(
    pipeline: list[NoCodeToolIn],
    session: Session,
    user: User,
    runtime_parameters: list[Parameter] | None = None,
    widget_id: str | None = None,
) -> RenderLoopResult:
    enriched_tools = enrich_tools_with_runtime_parameters(
        pipeline, runtime_parameters, widget_id
    )
    callable_pipeline = convert_to_callable_pipeline(enriched_tools)
    pipeline_output = evaluate_pipeline(callable_pipeline, session, user)
    result = serialize_to_result(pipeline_output)
    result_type = determine_result_type(result)
    return RenderLoopResult(
        result=result,
        result_type=result_type,
        parameters=extract_parameters_from_pipeline(pipeline, session, user),
    )


def _same(t1: type, t2: type) -> bool:
    if t1 == t2:
        return True
    o1, o2 = get_origin(t1), get_origin(t2)
    return o1 == o2 and all(
        _same(a, b) for a, b in zip(get_args(t1), get_args(t2), strict=False)
    )
