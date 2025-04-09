from dataclasses import dataclass
from decimal import Decimal
from typing import TypeVar

from app.schemas.no_code import Generator, Output, OutputType, Parameter, ParameterType, PipelineEnd,  Primitive, Transformation
from app.models import Transaction, TransactionId
from app.schemas.no_code import PipelineStart


@dataclass
class NoCodeTransaction:
    id: TransactionId|None
    amount: float
    description: str

class FirstNTransactionGenerator(Generator[NoCodeTransaction]):

    def __init__(self, kwargs: dict[str, int]):
        self.kwargs = kwargs

    @property    
    def parameters(self) -> list[Parameter]:
        return [Parameter(name="n", value=10, type=ParameterType.INT)]
    
    @property
    def output_type(self) -> type[Primitive[list[NoCodeTransaction]]]:
        return Primitive[list[NoCodeTransaction]]

    def call(self, start: PipelineStart) -> Primitive[list[NoCodeTransaction]]:
        n = self.kwargs["n"]
        transactions = start.session.query(Transaction).filter(Transaction.user_id == start.user.id).limit(n).all()
        return Primitive(name="transactions", value=[NoCodeTransaction(id=transaction.id, amount=transaction.amount, description=transaction.description) for transaction in transactions])

