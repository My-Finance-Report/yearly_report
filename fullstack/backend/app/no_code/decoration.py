from collections.abc import Callable
from dataclasses import dataclass
from typing import Any

from pydantic import TypeAdapter
from sqlalchemy.orm import Session
from app.models.no_code.parameter import ParameterType, SelectOption
from app.models.user import User
from app.schemas.no_code import NoCodeParameterCreate, NoCodeToolOut


@dataclass
class Arg:
    name: str
    type: ParameterType
    default_value: Any | None = None
    options_generator: Callable[[Session, User], list[SelectOption]] | None = None


PipelineCallable = Callable[..., Any]


@dataclass
class PipelineStep:
    func: PipelineCallable
    return_type: Any
    passed_value: Any
    parameters: list[NoCodeParameterCreate]


STEP_REGISTRY: dict[str, PipelineStep] = {}


def pipeline_step(
    return_type: Any,
    passed_value: Any = None,
    parameters: list[NoCodeParameterCreate] = [],
) -> Callable[[PipelineCallable], PipelineCallable]:
    def decorator(func: PipelineCallable) -> PipelineCallable:
        STEP_REGISTRY[func.__name__] = PipelineStep(
            func=func,
            return_type=return_type,
            passed_value=passed_value,
            parameters=parameters,
        )
        return func

    return decorator


def get_tool_callable(tool: str) -> PipelineCallable:
    if tool not in STEP_REGISTRY:
        raise ValueError(f"No such tool: {tool}")
    return STEP_REGISTRY[tool].func


def convert_type_to_json_schema(type_: Any) -> dict[str, Any]:
    adapter = TypeAdapter(type_)
    return adapter.json_schema()


def make_tools() -> list[NoCodeToolOut]:
    return [
        NoCodeToolOut(
            name=tool.func.__name__,
            description=tool.func.__doc__ or "",
            tool=tool.func.__name__,
            return_type=convert_type_to_json_schema(tool.return_type),
            parameters=tool.parameters,
        )
        for tool in STEP_REGISTRY.values()
    ]


def get_no_code_tool(
    tool_name: str,
) -> NoCodeToolOut:
    lookup = {tool.name: tool for tool in make_tools()}
    return lookup[tool_name]
