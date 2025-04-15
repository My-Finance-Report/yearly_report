from decimal import Decimal
from typing import TypeVar

from app.no_code.step import Kwargs, step
from app.schemas.no_code import OutputType, PipelineEnd, PrimitiveResult

T = TypeVar("T", bound=Decimal)


@step
def show_value(data: T, kwargs: Kwargs) -> PipelineEnd:
    return PipelineEnd(
        result=PrimitiveResult(name="show_value", value=data),
        output_type=OutputType.show_value,
    )


@step
def show_list(data: T, kwargs: Kwargs) -> PipelineEnd:
    return PipelineEnd(
        result=PrimitiveResult(name="show_list", value=data),
        output_type=OutputType.show_list,
    )
