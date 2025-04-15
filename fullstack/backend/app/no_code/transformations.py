from decimal import Decimal
from typing import TypeVar

from app.no_code.step import Kwargs, step
from app.schemas.no_code import NoCodeTransaction, Primitive

T = TypeVar("T", bound=Primitive[Decimal | NoCodeTransaction])


def get_value(value: T) -> Decimal:
    if isinstance(value, NoCodeTransaction):
        print("NoCodeTransaction", value.amount)
        return Decimal(value.amount)
    elif isinstance(value, Decimal):
        return value
    raise ValueError(f"Unsupported type: {type(value)}")


@step
def average_transform(data: list[T], kwargs: Kwargs) -> Decimal:
    return Decimal(sum([get_value(transaction) for transaction in data]))


@step
def sum_transform(data: list[T], kwargs: Kwargs) -> Decimal:
    return Decimal(sum([get_value(transaction) for transaction in data]))
