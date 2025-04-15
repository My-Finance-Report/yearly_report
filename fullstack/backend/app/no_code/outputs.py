from decimal import Decimal
from typing import TypeVar

from app.schemas.no_code import Output, OutputType, PipelineEnd,  Primitive, PrimitiveResult, PrimitiveResultValue
from app.no_code.generators import NoCodeTransaction

T = TypeVar("T", bound=Primitive[Decimal|NoCodeTransaction])

class ShowValue(Output[Primitive[T]]):
    @property
    def input_type(self) -> type[Primitive[T]]:
        return Primitive[T]
    
    def call(self, data: Primitive[T]) -> PipelineEnd:
        return PipelineEnd(result=PrimitiveResult(name="show_value", value=data.value), output_type=OutputType.show_value)

class ShowList(Output[Primitive[list[T]]]):
    @property
    def input_type(self) -> type[Primitive[list[T]]]:
        return Primitive[list[T]]
    
    def call(self, data: Primitive[list[T]]) -> PipelineEnd:
        return PipelineEnd(result=PrimitiveResult(name="show_list", value=data.value), output_type=OutputType.show_list)
    