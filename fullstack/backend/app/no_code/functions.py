from dataclasses import asdict, is_dataclass
from decimal import Decimal
from functools import partial
from inspect import isclass
from typing import Any, TypeVar, get_args, get_origin

from pydantic import BaseModel, Json


from app.db import Session
from app.models import TransactionSource, User
from app.schemas.no_code import (
    SelectOption,
    NoCodeToolIn,
    PipelineStart,
    ResultType,
)
from app.no_code.decoration import get_tool_callable

T = TypeVar("T")

def make_account_choices(session: Session,user: User)->list[SelectOption]:
    accts = session.query(TransactionSource.name, TransactionSource.id).filter(TransactionSource.user_id == user.id, ~TransactionSource.archived).all()
    return [SelectOption(key=acct.id, value=acct.name) for acct in accts]


def init_no_code()->None:
    import app.no_code.transformations
    import app.no_code.generators


def convert_to_pipeline(tools: list[NoCodeToolIn]) -> list[partial[Any]]:
    steps = []
    for tool in tools:
        func = get_tool_callable(tool.tool)
        if tool.parameters:
            kwargs = {p.name: p.value for p in tool.parameters}
            steps.append(partial(func, kwargs=kwargs))
        else:
            steps.append(partial(func, kwargs={}))

    return steps


def serialize_to_result(obj: Any) -> Json:
    if isinstance(obj, (Decimal, str, int, float)):
        return obj
    elif isinstance(obj, BaseModel):
        return obj.model_dump()
    elif is_dataclass(obj):
        if not isinstance(obj, type):
            return {
                k: serialize_to_result(v)
                for k, v in asdict(obj).items()
            }
    elif isinstance(obj, dict):
        return {k: serialize_to_result(v) for k, v in obj.items()}
    elif isinstance(obj, (list, tuple)):
        return [serialize_to_result(item) for item in obj]
    else:
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
