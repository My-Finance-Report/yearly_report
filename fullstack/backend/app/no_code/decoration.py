from collections.abc import Callable
from dataclasses import dataclass
from typing import Any

from pydantic import TypeAdapter
from sqlalchemy.orm import Session

from app.models import User
from app.schemas.no_code import NoCodeTool, Parameter, ParameterType, SelectOption


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


STEP_REGISTRY: dict[str, PipelineStep] = {}


def pipeline_step(
    return_type: Any, passed_value: Any = None
) -> Callable[[PipelineCallable], PipelineCallable]:
    def decorator(func: PipelineCallable) -> PipelineCallable:
        STEP_REGISTRY[func.__name__] = PipelineStep(
            func=func,
            return_type=return_type,
            passed_value=passed_value,
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


def make_tools(
    session: Session,
    user: User,
) -> list[NoCodeTool]:
    tools: list[NoCodeTool] = []
    return tools

    # need to figure out some registry scheme for the parameters

    """
    for name, tool in STEP_REGISTRY.items():
        tools.append(
            NoCodeTool(
                name=name,
                description=tool.func.__doc__ or "",
                tool=name,
                input_type=convert_type_to_json_schema(tool.passed_value),
                return_type=convert_type_to_json_schema(tool.return_type),
                parameters=[
                    Parameter(
                        name=arg.name,
                        type=arg.type,
                        value=arg.default_value,
                        options=arg.options_generator(session, user)
                        if arg.options_generator
                        else None,
                    )
                    for arg in tool.expected_args
                ],
            )
        )
    return tools
    """


def get_no_code_tool(
    session: Session,
    user: User,
    tool_name: str,
) -> NoCodeTool:
    lookup = {tool.name: tool for tool in make_tools(session, user)}
    return lookup[tool_name]
