from typing import  NewType
from decimal import Decimal
from app.models.category import CategoryId
from app.models.models import Base
from sqlalchemy import (
    Boolean,
    ForeignKey,
    UniqueConstraint,
    Text,
    Numeric,
    Integer,
)
from sqlalchemy.orm import Mapped, mapped_column

from app.models.user import UserId




BudgetId = NewType("BudgetId", int)
BudgetEntryId = NewType("BudgetEntryId", int)
BudgetCategoryLinkId = NewType("BudgetCategoryLinkId", int)



class Budget(Base):
    __tablename__ = "budget"

    id: Mapped[BudgetId] = mapped_column(Integer, primary_key=True, autoincrement=True)
    name: Mapped[str] = mapped_column(Text, nullable=False)
    user_id: Mapped[UserId] = mapped_column(ForeignKey("user.id"), nullable=False)
    active: Mapped[bool] = mapped_column(Boolean, nullable=False)

    __table_args__ = (UniqueConstraint("user_id", "active", name="uq_budget"),)


class BudgetEntry(Base):
    __tablename__ = "budget_entry"

    id: Mapped[BudgetEntryId] = mapped_column(
        Integer, primary_key=True, autoincrement=True
    )
    user_id: Mapped[UserId] = mapped_column(ForeignKey("user.id"), nullable=False)
    name: Mapped[str] = mapped_column(Text, nullable=False)
    amount: Mapped[Decimal] = mapped_column(Numeric, nullable=False)
    budget_id: Mapped[BudgetId] = mapped_column(ForeignKey("budget.id"), nullable=False)


class BudgetCategoryLink(Base):
    __tablename__ = "budget_category_link"

    id: Mapped[BudgetCategoryLinkId] = mapped_column(
        Integer, primary_key=True, autoincrement=True
    )
    user_id: Mapped[UserId] = mapped_column(ForeignKey("user.id"), nullable=False)
    budget_entry_id: Mapped[BudgetEntryId] = mapped_column(
        ForeignKey("budget_entry.id"), nullable=False
    )
    category_id: Mapped[CategoryId] = mapped_column(
        ForeignKey("category.id"), nullable=False
    )

