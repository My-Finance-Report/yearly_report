from dataclasses import dataclass
from app.schemas.no_code import Generator,  Primitive, Transformation
from app.core.db import Session
from app.models import Transaction, TransactionId, User


@dataclass
class NoCodeTransaction:
    id: TransactionId|None
    amount: float
    description: str

class FirstTenTransactionGenerator(Generator[NoCodeTransaction]):
    @property
    def output_type(self) -> type[NoCodeTransaction]:
        return NoCodeTransaction

    def generate(self, user: User, session: Session) -> list[Primitive[NoCodeTransaction]]:
        transactions = session.query(Transaction).filter(Transaction.user_id == user.id).limit(100).all()
        return [Primitive(name=str(transaction.id), value=NoCodeTransaction(id=transaction.id, amount=transaction.amount, description=transaction.description)) for transaction in transactions]


class SumTransformation(Transformation[NoCodeTransaction]):
    @property
    def input_type(self) -> type[NoCodeTransaction]:
        return NoCodeTransaction

    @property
    def output_type(self) -> type[NoCodeTransaction]:
        return NoCodeTransaction

    def transform(self, user: User, session: Session, data: list[Primitive[NoCodeTransaction]]) -> list[Primitive[NoCodeTransaction]]:
        return [Primitive(name="sum", value=NoCodeTransaction(id=None, amount=sum(transaction.amount for transaction in data), description="Sum of transactions"))]
    

class AverageTransformation(Transformation[NoCodeTransaction]):
    @property
    def input_type(self) -> type[NoCodeTransaction]:
        return NoCodeTransaction

    @property
    def output_type(self) -> type[NoCodeTransaction]:
        return NoCodeTransaction

    def transform(self, user: User, session: Session, data: list[Primitive[NoCodeTransaction]]) -> list[Primitive[NoCodeTransaction]]:
        return [Primitive(name="average", value=NoCodeTransaction(id=None, amount=sum(transaction.amount for transaction in data) / len(data), description="Average of transactions"))]
    
