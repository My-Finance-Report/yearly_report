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
    val = Decimal(sum([get_value(transaction) for transaction in data]))
    return val / len(data)

@step
def sum_transform(data: list[T], kwargs: Kwargs) -> Decimal:
    return Decimal(sum([get_value(transaction) for transaction in data]))

@step
def to_key_value_pair(data: list[T], kwargs: Kwargs) -> list[dict]:
    key_from = str(kwargs["key_from"])
    value_from = str(kwargs["value_from"])
    return [{"key": getattr(transaction, key_from), "value": getattr(transaction, value_from)} for transaction in data]
