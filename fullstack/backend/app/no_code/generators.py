from dataclasses import dataclass
from decimal import Decimal
from typing import TypeVar

from app.schemas.no_code import Generator, Output, OutputType, PipelineEnd,  Primitive, Transformation
from app.models import Transaction, TransactionId
from app.schemas.no_code import PipelineStart


@dataclass
class NoCodeTransaction:
    id: TransactionId|None
    amount: float
    description: str

class FirstTenTransactionGenerator(Generator[NoCodeTransaction]):
    @property
    def output_type(self) -> type[Primitive[list[NoCodeTransaction]]]:
        return Primitive[list[NoCodeTransaction]]

    def call(self, start: PipelineStart) -> Primitive[list[NoCodeTransaction]]:
        transactions = start.session.query(Transaction).filter(Transaction.user_id == start.user.id).limit(100).all()
        return Primitive(name="transactions", value=[NoCodeTransaction(id=transaction.id, amount=transaction.amount, description=transaction.description) for transaction in transactions])

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



class ShowValue(Output[Primitive[T]]):
    @property
    def input_type(self) -> type[Primitive[T]]:
        return Primitive[T]
    
    def call(self, data: Primitive[T]) -> PipelineEnd:
        return PipelineEnd(result=Primitive(name="show_value", value=data), output_type=OutputType.show_value)
    


class ShowList(Output[Primitive[list[T]]]):
    @property
    def input_type(self) -> type[Primitive[list[T]]]:
        return Primitive[list[T]]
    
    def call(self, data: Primitive[list[T]]) -> PipelineEnd:
        return PipelineEnd(result=Primitive(name="show_list", value=data.value), output_type=OutputType.show_list)
    
