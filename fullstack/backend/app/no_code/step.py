import functools
import inspect
from collections.abc import Callable
from typing import (
    Generic,
    ParamSpec,
    Protocol,
    TypeVar,
    get_type_hints,
    runtime_checkable,
)

T_in = TypeVar("T_in", contravariant=True)  # data *comes in* → contra
T_out = TypeVar("T_out", covariant=True)  # data *goes out* →  co

Kwargs = dict[str, int | float | str | None]


@runtime_checkable
class Step(Protocol[T_in, T_out]):
    def __call__(self, data: T_in, kwargs: Kwargs) -> T_out: ...


P = ParamSpec("P")
R = TypeVar("R")


class FnStep(Generic[P, R]):
    """Wrap a callable so it behaves like a `Step`."""

    input_type: type
    output_type: type[R]

    def __init__(self, func: Callable[P, R]) -> None:
        functools.update_wrapper(self, func)
        self._func = func

        hints = get_type_hints(func)
        sig = inspect.signature(func)
        first_param = next(iter(sig.parameters.values()))
        self.__class__.input_type = hints[first_param.name]
        self.__class__.output_type = hints["return"]

    def __call__(self, *args: P.args, **kwargs: P.kwargs) -> R:
        return self._func(*args, **kwargs)


def step(fn: Callable[P, R]) -> FnStep[P, R]:
    return FnStep(fn)
