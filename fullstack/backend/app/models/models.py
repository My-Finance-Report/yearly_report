from typing import Generic, TypeVar

from pydantic import BaseModel
from sqlalchemy import (
    JSON,
    Dialect,
    TypeDecorator,
)
from sqlalchemy.orm import DeclarativeBase


T = TypeVar("T", bound=BaseModel)  # This represents any dataclass type


class JSONType(TypeDecorator[T], Generic[T]):
    """
    A generic SQLAlchemy column type that stores any dataclass as JSON.
    """

    impl = JSON

    def __init__(self, dataclass_type: type[T]) -> None:
        """Initialize with the dataclass type that should be used for deserialization."""
        super().__init__()
        self.dataclass_type = dataclass_type

    def process_bind_param(self, value: T | None, _dialect: Dialect) -> str | None:
        """Convert a BaseModel into JSON when writing to the database."""
        if value is not None:
            return value.model_dump_json()
        return None

    def process_result_value(
        self, value: dict[str, str] | str | bytes | bytearray | None, _dialect: Dialect
    ) -> T | None:
        """Convert JSON from the database back into the correct dataclass."""
        if value is None:
            return None

        if isinstance(value, dict):
            # this is a failover from previous json handling
            return self.dataclass_type.model_validate(value)
        else:
            # If we have a string/bytes, use model_validate_json
            return self.dataclass_type.model_validate_json(value)


class Base(DeclarativeBase):
    __rls_enabled__ = True






