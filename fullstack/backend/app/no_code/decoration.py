from ast import Call
from dataclasses import dataclass
from typing import Any, Callable

from sqlalchemy.orm import Session

from app.models import User
from app.schemas.no_code import NoCodeTool, Parameter, ParameterType, SelectOption


@dataclass
class Arg:
    name: str
    type: ParameterType
    default_value: Any | None = None
    options_generator: Callable[[Session, User], list[SelectOption]] | None = None

@dataclass
class PipelineStep:
    func: Callable 
    expected_args: list[Arg]
    return_type: Any
    passed_value: Any

STEP_REGISTRY: dict[str, PipelineStep] = {}

print("just initalized the registry")

def pipeline_step(
    expected_kwargs: list[Arg],
    return_type: Any,
    passed_value: Any = None
)-> Callable[[Callable], Callable]:
    print("make it here")
    def decorator(func: Callable) -> Callable:

        print("make it here there")
        STEP_REGISTRY[func.__name__] = PipelineStep(
            func=func,
            expected_args=expected_kwargs,
            return_type=return_type,
            passed_value=passed_value
        )
        return func
    return decorator


def get_tool_callable(tool:str)->Callable:
    if tool not in STEP_REGISTRY:
        raise ValueError(f"No such tool: {tool}")
    return STEP_REGISTRY[tool].func

def make_tools(
    session: Session,
    user: User,
) -> list[NoCodeTool]:

    tools= []
    print(STEP_REGISTRY)
    for name,tool in STEP_REGISTRY.items():
        tools.append(
            NoCodeTool(
                name=name,
                description=tool.func.__doc__ or "",
                tool=name,
                parameters=[Parameter(name=arg.name, type=arg.type, value=arg.default_value, options=arg.options_generator(session, user) if arg.options_generator else None) for arg in tool.expected_args]
            )
        )
    print(tools)
    return tools

