import logging
from collections.abc import Callable
from typing import TypeVar

T = TypeVar("T")
J = TypeVar("J")


def make_batches(data: list[T], batch_size: int = 10) -> list[list[T]]:
    return [data[i : i + batch_size] for i in range(0, len(data), batch_size)]


logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


def pipe(value: T, *funcs: Callable[[T], T], final: Callable[[T], J]) -> J:
    for func in funcs:
        print(func)
        value = func(value)
    return final(value)
