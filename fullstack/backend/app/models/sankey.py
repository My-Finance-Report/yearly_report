from app.models.category import CategoryId
from app.models.models import Base
from sqlalchemy import (
    ForeignKey,
    Text,
    Integer,
)
from sqlalchemy.orm import Mapped, mapped_column
from app.models.transaction_source import TransactionSourceId

from app.models.user import UserId


class SankeyConfig(Base):
    __tablename__ = "sankey_config"

    id: Mapped[int] = mapped_column(Integer, primary_key=True, autoincrement=True)
    name: Mapped[str] = mapped_column(Text, nullable=False)
    user_id: Mapped[UserId] = mapped_column(ForeignKey("user.id"), nullable=False)


class SankeyInput(Base):
    __tablename__ = "sankey_input"

    id: Mapped[int] = mapped_column(Integer, primary_key=True, autoincrement=True)
    config_id: Mapped[int] = mapped_column(
        ForeignKey("sankey_config.id"), nullable=False
    )
    category_id: Mapped[CategoryId] = mapped_column(
        ForeignKey("category.id"), nullable=False
    )


class SankeyLinkage(Base):
    __tablename__ = "sankey_linkage"

    id: Mapped[int] = mapped_column(Integer, primary_key=True, autoincrement=True)
    config_id: Mapped[int] = mapped_column(
        ForeignKey("sankey_config.id"), nullable=False
    )
    category_id: Mapped[CategoryId] = mapped_column(
        ForeignKey("category.id"), nullable=False
    )
    target_source_id: Mapped[TransactionSourceId] = mapped_column(
        ForeignKey("transaction_source.id"), nullable=False
    )
