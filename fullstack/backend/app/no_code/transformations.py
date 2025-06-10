from datetime import datetime
from collections import defaultdict
from decimal import Decimal
from typing import TypeVar

from pydantic import BaseModel
from sqlalchemy.orm import Session
from app.models.no_code.parameter import SelectOption

from app.no_code.decoration import pipeline_step
from app.schemas.no_code import NoCodeTransaction, Primitive
from app.models.user import User

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
def average_transform(data: list[NoCodeTransaction] | list[Decimal]) -> Decimal:
    val = Decimal(sum([get_value(transaction) for transaction in data]))
    return val / len(data)


@pipeline_step(
    return_type=Decimal,
    passed_value=list[NoCodeTransaction] | list[Decimal],
)
def sum_transform(data: list[NoCodeTransaction] | list[Decimal]) -> Decimal:
    return Decimal(sum([get_value(transaction) for transaction in data]))


class KeyValuePair(BaseModel):
    key: str
    value: str | Decimal | None


def parse_key(value: str | Decimal | datetime) -> str:
    if isinstance(value, str):
        return value
    if isinstance(value, float):
        return str(value)
    elif isinstance(value, Decimal):
        return str(value)
    elif isinstance(value, datetime):
        return value.strftime("%Y-%m-%d")


def parse_value(value: str | Decimal | datetime | None) -> str | Decimal | None:
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
            key=parse_key(getattr(transaction, key_from)),
            value=parse_value(getattr(transaction, value_from)),
        )
        for transaction in data
    ]


def make_group_bys(_session: Session, _user: User) -> list[SelectOption]:
    return [
        SelectOption(key="day", value="Day"),
        SelectOption(key="month", value="Month"),
        SelectOption(key="year", value="Year"),
    ]


@pipeline_step(
    return_type=list[dict[str, list[NoCodeTransaction]]],
    passed_value=list[NoCodeTransaction],
)
def aggregate(
    data: list[NoCodeTransaction],
    key_from: SelectOption,
    values_from: list[SelectOption],
) -> list[dict[str, str | Decimal | None]]:
    result: dict[str, dict[str, str | Decimal | None]] = {}

    for transaction in data:
        key = parse_key(getattr(transaction, key_from.key))
        if key not in result:
            result[key] = {}
            result[key][key_from.key] = key

        if isinstance(values_from, list):  # TODO figure out why this type is wrong
            for value in values_from:
                result[key][value.key] = parse_value(getattr(transaction, value.key))
        else:
            result[key][values_from.key] = parse_value(
                getattr(transaction, values_from.key)
            )

    return list(result.values())


@pipeline_step(
    return_type=list[dict[str, str | Decimal | None]],
    passed_value=list[dict[str, str | Decimal | None]],
)
def sum_by_key(
    data: list[NoCodeTransaction], key_from: SelectOption, values_from: SelectOption
) -> list[KeyValuePair]:
    result: dict[str, float] = defaultdict(lambda: float(0))

    for transaction in data:
        key = parse_key(getattr(transaction, key_from.key))
        result[key] += (
            getattr(transaction, values_from.key)
            if getattr(transaction, values_from.key) is not None
            else 0.0
        )

    return [
        KeyValuePair(key=key, value=Decimal(value)) for key, value in result.items()
    ]


@pipeline_step(
    return_type=list[dict[str, str | Decimal | None]],
    passed_value=list[dict[str, str | Decimal | None]],
)
def clean_transaction_data(data: list[NoCodeTransaction]) -> list[dict[str, str]]:
    result: list[dict[str, str]] = []

    for transaction in data:
        result.append(
            {
                "Date": transaction.date_of_transaction.strftime("%m/%d/%Y"),
                "Description": transaction.description,
                "Amount": f"${transaction.amount:.2f}",
                "Category": transaction.category_name,
                "Kind": transaction.kind.value.title(),
            }
        )

    return result
