from dataclasses import dataclass
from decimal import Decimal
from typing import TypeVar
from app.schemas.no_code import Generator, OutputType, PipelineEnd,  Primitive, Transformation
from app.models import Transaction, TransactionId
from app.schemas.no_code import PipelineStart


@dataclass
class NoCodeTransaction:
    id: TransactionId|None
    amount: float
    description: str

class FirstTenTransactionGenerator(Generator[NoCodeTransaction]):
    def input_type(self) -> type[NoCodeTransaction]:
        return NoCodeTransaction
    
    def output_type(self) -> type[list[Primitive[NoCodeTransaction]]]:
        return list[Primitive[NoCodeTransaction]]

    def call(self, start: PipelineStart) -> list[Primitive[NoCodeTransaction]]:
        transactions = start.session.query(Transaction).filter(Transaction.user_id == start.user.id).limit(100).all()
        return [Primitive(name=str(transaction.id), value=NoCodeTransaction(id=transaction.id, amount=transaction.amount, description=transaction.description)) for transaction in transactions]

T = TypeVar("T", bound=Primitive[Decimal] | NoCodeTransaction)

class SumTransformation(Transformation[list[Primitive[T]], Primitive[Decimal]]):
    @property
    def input_type(self) -> type[list[Primitive[T]]]:
        return list[Primitive[T]]
    
    @property
    def output_type(self) -> type[Primitive[Decimal]]:
        return Primitive[Decimal]
    
    def get_summable_value(self, value:Primitive[T]) -> Decimal:
        if isinstance(value.value, NoCodeTransaction):
            return Decimal(value.value.amount)
        elif isinstance(value.value, Decimal):
            return value.value
        raise ValueError(f"Unsupported type: {type(value.value)}")

    def call(self, data: list[Primitive[Decimal]]) -> Primitive[Decimal]:
        return Primitive(name="sum", value=Decimal(sum([self.get_summable_value(transaction) for transaction in data])))
    

class AverageTransformation(Transformation[list[Primitive[T]], Primitive[Decimal]]):
    @property
    def input_type(self) -> type[list[Primitive[T]]]:
        return list[Primitive[T]]
    
    @property
    def output_type(self) -> type[Primitive[Decimal]]:
        return Primitive[Decimal]
    

    def get_averageable_value(self, value:Primitive[T]) -> Decimal:
        if isinstance(value.value, NoCodeTransaction):
            return Decimal(value.value.amount)
        elif isinstance(value.value, Decimal):
            return value.value
        raise ValueError(f"Unsupported type: {type(value.value)}")

    def call(self, data: list[Primitive[T]]) -> Primitive[Decimal]:
        return Primitive(name="average", value=Decimal(sum([self.get_averageable_value(transaction) for transaction in data])) / len(data))



class ShowValue(Transformation[Primitive[T], PipelineEnd]):
    @property
    def input_type(self) -> type[Primitive[T]]:
        return Primitive[T]
    
    @property
    def output_type(self) -> type[PipelineEnd]:
        return PipelineEnd

    def call(self, data: Primitive[T]) -> PipelineEnd:
        return PipelineEnd(result=Primitive(name="show_value", value=data), output_type=OutputType.show_value)
    


class ShowList(Transformation[list[Primitive[T]], PipelineEnd]):
    @property
    def input_type(self) -> type[list[Primitive[T]]]:
        return list[Primitive[T]]
    
    @property
    def output_type(self) -> type[PipelineEnd]:
        return PipelineEnd

    def call(self, data: list[Primitive[T]]) -> PipelineEnd:
        return PipelineEnd(result=Primitive(name="show_list", value=data), output_type=OutputType.show_list)
    
