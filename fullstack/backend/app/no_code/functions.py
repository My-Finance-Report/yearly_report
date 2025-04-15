from collections.abc import Callable
from dataclasses import asdict, dataclass, is_dataclass
from decimal import Decimal
from functools import partial
from typing import Any, TypeVar, get_args, get_origin


from app.db import Session
from app.models import TransactionSource, User
from app.no_code.generators import account_balance, account_name, first_n_transactions
from app.no_code.transformations import average_transform, sum_transform, to_key_value_pair
from app.schemas.no_code import (
    NoCodeTool,
    SelectOption,
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
    ToolType.account_name: account_name,
    ToolType.account_balance: account_balance,
    ToolType.to_key_value_pair: to_key_value_pair,
}


def make_account_choices(session: Session,user: User)->list[SelectOption]:
    accts = session.query(TransactionSource.name, TransactionSource.id).filter(TransactionSource.user_id == user.id, ~TransactionSource.archived).all()
    return [SelectOption(key=acct.id, value=acct.name) for acct in accts]

def make_tools(
    session: Session,
    user: User,
) -> list[NoCodeTool]:
    return [
        NoCodeTool(
            name="First n Transactions",
            description="Get the first n transactions for the user",
            tool=ToolType.first_ten_transactions,
            parameters=[Parameter(name="n", type=ParameterType.INT),Parameter(name="account", type=ParameterType.SELECT, options=make_account_choices(session, user))],
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
            name="Account Name",
            description= "simply returns the account name",
            tool=ToolType.account_name,
            parameters=[Parameter(name="account", type=ParameterType.SELECT, options=make_account_choices(session,user))]
        ),
        NoCodeTool(
            name="Account Balance",
            description= "simply returns the account balance",
            tool=ToolType.account_balance,
            parameters=[Parameter(name="account", type=ParameterType.SELECT, options=make_account_choices(session,user))]
        ),
        NoCodeTool(
            name="to key value pairs",
            description="todo",
            tool = ToolType.to_key_value_pair,
            parameters=[Parameter(name="key_from", type=ParameterType.STRING, value="category_name"), Parameter(name="value_from", type=ParameterType.STRING, value="amount")]
            
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
