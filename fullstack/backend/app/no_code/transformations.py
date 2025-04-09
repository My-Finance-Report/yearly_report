from decimal import Decimal
from typing import TypeVar

from app.schemas.no_code import Primitive, Transformation
from app.no_code.generators import NoCodeTransaction

T = TypeVar("T", bound=Primitive[Decimal|NoCodeTransaction])

class SumTransformation(Transformation[Primitive[list[T]], Primitive[Decimal]]):
    @property
    def input_type(self) -> type[Primitive[list[T]]]:
        return Primitive[list[T]]
    
    @property
    def output_type(self) -> type[Primitive[Decimal]]:
        return Primitive[Decimal]
    
    def get_summable_value(self, value:Primitive[T]) -> Decimal:
        if isinstance(value, NoCodeTransaction):
            return Decimal(value.amount)
        elif isinstance(value, Decimal):
            return value
        raise ValueError(f"Unsupported type: {type(value)}")

    def call(self, data: Primitive[list[T]]) -> Primitive[Decimal]:
        return Primitive(name="sum", value=Decimal(sum(self.get_summable_value(value=transaction) for transaction in data.value)))
    

class AverageTransformation(Transformation[Primitive[list[T]], Primitive[Decimal]]):
    @property
    def input_type(self) -> type[Primitive[list[T]]]:
        return Primitive[list[T]]
    
    @property
    def output_type(self) -> type[Primitive[Decimal]]:
        return Primitive[Decimal]

    def get_averageable_value(self, value:Primitive[T]) -> Decimal:
        if isinstance(value, NoCodeTransaction):
            return Decimal(value.amount)
        elif isinstance(value, Decimal):
            return value
        raise ValueError(f"Unsupported type: {type(value)}")

    def call(self, data: Primitive[list[T]]) -> Primitive[Decimal]:
        return Primitive(name="average", value=Decimal(sum([self.get_averageable_value(transaction) for transaction in data.value])) / len(data.value))

