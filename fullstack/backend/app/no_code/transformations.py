from collections import defaultdict
from datetime import datetime
from decimal import Decimal
from typing import Callable, TypeVar

from pydantic import BaseModel
from sqlalchemy.orm import Session

from app.no_code.decoration import Arg, pipeline_step
from app.no_code.step import Kwargs
from app.schemas.no_code import NoCodeTransaction, ParameterType, Primitive, SelectOption
from app.models import User
from app.api.routes.no_code import safe_parse_int

T = TypeVar("T", bound=Primitive[Decimal | NoCodeTransaction])


def get_value(value: Decimal | NoCodeTransaction) -> Decimal:
    if isinstance(value, NoCodeTransaction):
        print("NoCodeTransaction", value.amount)
        return Decimal(value.amount)
    elif isinstance(value, Decimal):
        return value
    raise ValueError(f"Unsupported type: {type(value)}")


@pipeline_step(
    return_type=Decimal,
    passed_value=list[NoCodeTransaction] | list[Decimal],
)
def average_transform(
    data: list[NoCodeTransaction] | list[Decimal]
) -> Decimal:
    val = Decimal(sum([get_value(transaction) for transaction in data]))
    return val / len(data)


@pipeline_step(
    return_type=Decimal,
    passed_value=list[NoCodeTransaction] | list[Decimal],
)
def sum_transform(
    data: list[NoCodeTransaction] | list[Decimal]
) -> Decimal:
    return Decimal(sum([get_value(transaction) for transaction in data]))


class KeyValuePair(BaseModel):
    key: str 
    value: str | Decimal | None


def parse_key(value: str | Decimal | datetime) -> str:
    if isinstance(value, str):
        return value
    if isinstance(value, float):
        return Decimal(value)
    elif isinstance(value, Decimal):
        return str(value)
    elif isinstance(value, datetime):
        return value.strftime("%Y-%m-%d")

def parse_value(value: str | Decimal | datetime | None) -> str | Decimal |None:
    if isinstance(value, str):
        return value
    if isinstance(value, float):
        return Decimal(value)
    elif isinstance(value, Decimal):
        return value
    elif isinstance(value, datetime):
        return value.strftime("%Y-%m-%d")
    return None


@pipeline_step(
    return_type=list[KeyValuePair],
    passed_value=list[NoCodeTransaction],
)
def to_key_value_pair(
    data: list[NoCodeTransaction], key_from: str, value_from: str
) -> list[KeyValuePair]:
    return [
        KeyValuePair(
            key=parse_key(getattr(transaction, key_from)), value=parse_value(getattr(transaction, value_from))
        )
        for transaction in data
    ]


def make_group_bys(_session:Session, _user:User)->list[SelectOption]:
    return [
        SelectOption(key=0, value="Day"),
        SelectOption(key=1, value="Month"),
        SelectOption(key=2, value="Year"),
    ]


@pipeline_step(
    return_type=list[dict[str,list[NoCodeTransaction]]],
    passed_value=list[NoCodeTransaction],
)
def aggregate(
    data: list[NoCodeTransaction], key_from: str, values_from: list[str]
) -> list[dict[str,str | Decimal | None]]:
    result: dict[str, dict[str,str | Decimal | None]] = {}

    for transaction in data:
        key = parse_key(getattr(transaction, key_from))
        if key not in result:
            result[key] = {}
            result[key][key_from] = key

        for value in values_from:
            result[key][getattr(transaction, 'description')] = parse_value(getattr(transaction, value))

    return list(result.values())
