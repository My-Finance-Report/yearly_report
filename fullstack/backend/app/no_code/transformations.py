from decimal import Decimal
from typing import TypeVar

from pydantic import BaseModel

from app.no_code.decoration import Arg, pipeline_step
from app.no_code.step import Kwargs
from app.schemas.no_code import NoCodeTransaction, ParameterType, Primitive

T = TypeVar("T", bound=Primitive[Decimal | NoCodeTransaction])


def get_value(value: Decimal | NoCodeTransaction) -> Decimal:
    if isinstance(value, NoCodeTransaction):
        print("NoCodeTransaction", value.amount)
        return Decimal(value.amount)
    elif isinstance(value, Decimal):
        return value
    raise ValueError(f"Unsupported type: {type(value)}")


@pipeline_step(
    expected_kwargs=[],
    return_type=Decimal,
    passed_value=list[NoCodeTransaction] | list[Decimal],
)
def average_transform(
    data: list[NoCodeTransaction] | list[Decimal], kwargs: Kwargs
) -> Decimal:
    val = Decimal(sum([get_value(transaction) for transaction in data]))
    return val / len(data)


@pipeline_step(
    expected_kwargs=[],
    return_type=Decimal,
    passed_value=list[NoCodeTransaction] | list[Decimal],
)
def sum_transform(
    data: list[NoCodeTransaction] | list[Decimal], kwargs: Kwargs
) -> Decimal:
    return Decimal(sum([get_value(transaction) for transaction in data]))


class KeyValuePair(BaseModel):
    key: str | Decimal
    value: str | Decimal


@pipeline_step(
    expected_kwargs=[
        Arg(name="key_from", type=ParameterType.STRING),
        Arg(name="value_from", type=ParameterType.STRING),
    ],
    return_type=list[KeyValuePair],
    passed_value=list[NoCodeTransaction],
)
def to_key_value_pair(
    data: list[NoCodeTransaction], kwargs: Kwargs
) -> list[KeyValuePair]:
    key_from = str(kwargs["key_from"])
    value_from = str(kwargs["value_from"])
    return [
        KeyValuePair(
            key=getattr(transaction, key_from), value=getattr(transaction, value_from)
        )
        for transaction in data
    ]
