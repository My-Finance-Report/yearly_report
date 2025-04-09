
from abc import ABC
from typing import  Callable, TypeVar, Any
from app.schemas.no_code import Generator, Output, Parameter, ParameterType, PipelineEnd, PipelineStart, Primitive, Transformation
from app.no_code.generators import FirstNTransactionGenerator
from app.no_code.outputs import ShowValue, ShowList
from app.no_code.transformations import SumTransformation, AverageTransformation
import enum
from pydantic import BaseModel

from app.db import Session
from app.models import User

T = TypeVar("T")


class ToolType(str, enum.Enum):
    first_ten_transactions = "first_ten_transactions"
    sum = "sum"
    average = "average"
    show_value = "show_value"
    show_list = "show_list"


class NoCodeTool(BaseModel):
    name: str
    description: str
    tool: ToolType
    parameters: list[Parameter] | None =None


tool_type_map = {
    ToolType.first_ten_transactions: lambda params: FirstNTransactionGenerator({params.name : params.value for params in params}),
    ToolType.sum: lambda _params: SumTransformation(),
    ToolType.average: lambda _params: AverageTransformation(),
    ToolType.show_value: lambda _params: ShowValue(),
    ToolType.show_list: lambda _params: ShowList(),
}



def make_tools()->list[NoCodeTool]:
    return [
        NoCodeTool(
            name="First n Transactions",
            description="Get the first n transactions for the user",
            tool=ToolType.first_ten_transactions,
            parameters=[Parameter(name="n", type=ParameterType.INT)],
        ),
        NoCodeTool(
            name="Sum",
            description="Sum of the passed values",
            tool=ToolType.sum,
        ),
        NoCodeTool(
            name="Average",
            description="Average of the passed values",
            tool=ToolType.average,
        ),
        NoCodeTool(
            name="Show Value",
            description="Show the passed value",
            tool=ToolType.show_value,
        ),
        NoCodeTool(
            name="Show List",
            description="Show the passed list",
            tool=ToolType.show_list,
        ),
    ]



def convert_to_pipeline(pipeline: list[NoCodeTool]) -> tuple[Generator[T], list[Transformation[T,T]], Output[T]]:
    pipe = [tool_type_map[tool.tool](tool.parameters) for tool in pipeline]
    lint_pipeline(pipe)


    # we can ignore because we know the types from linting
    return pipe[0], pipe[1:-1], pipe[-1] #type: ignore 



def evaluate_pipeline(first: Generator[T], steps: list[Transformation[T,T]], last: Output[T], session: Session, user: User)-> PipelineEnd:

    args: PipelineStart = PipelineStart(user, session)
    curr_args = first.call(args)
    blah: T = curr_args
    for step in steps:
        blah = step.call(blah)

    return last.call(blah)
    
        
def lint_pipeline(steps: list[ABC]) -> None:
    """
    Checks that each step's input_type matches the previous step's output_type.
    Raises a ValueError if there's a mismatch.
    """
    if not steps:
        raise ValueError("No steps in the pipeline.")

    first = steps[0]
    if not hasattr(first, "output_type"):
        raise ValueError("First step must have an 'output_type' (e.g., a Generator).")

    current_type = getattr(first, "output_type", None)

    for i in range(1, len(steps)):
        step = steps[i]
        expected_input = getattr(step, "input_type", None)
        if expected_input is None:
            raise ValueError(f"Step {i} ({step.__class__.__name__}) has no 'input_type' declared.")

        valid, message = valid_handoff(current_type, expected_input)
        if not valid:
            raise ValueError(
                f"Type mismatch at step {i} ({step.__class__.__name__}): "
                f"{message}"
            )

        if hasattr(step, "output_type"):
            current_type = getattr(step, "output_type", None)

   


def valid_handoff(pass_from: Any, pass_to: Any) -> tuple[bool, str | None]:
    if pass_from == pass_to:
        return True, None
    if "Primitive[list[" in str(pass_from) and "Primitive[list[" in str(pass_to):
        return True, None
    if "Primitive[list" in str(pass_from) and "Primitive[list" not in str(pass_to):
        return False, "Cannot pass a list to a step that doesn't accept a list"
    if "Primitive[list"not in str(pass_from) and "Primitive[list" in str(pass_to):
        return False, "Cannot pass a non-list to a step that requires a list"
    if "Primitive[" in str(pass_from) and "Primitive[" not in str(pass_to):
        return False, "Cannot pass a non-list to a step that requires a list"
    if "Primitive[" not in str(pass_from) and "Primitive[" in str(pass_to):
        return False, "Cannot pass a non-list to a step that requires a list"
    return True, None