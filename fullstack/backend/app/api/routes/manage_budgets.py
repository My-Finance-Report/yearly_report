from fastapi import APIRouter, Depends
from sqlalchemy.orm import Session

from app.db import get_current_user, get_db
from app.local_types import BudgetBase, BudgetEntryBase, BudgetEntryOut, BudgetOut
from app.models import Budget, BudgetCategoryLink, BudgetEntry , User

router = APIRouter(prefix="/budgets", tags=["budgets"])


@router.get("/", response_model=list[Budget])
def get_budgets(
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> list[Budget]:
    return (
        session.query(Budget)
        .filter(Budget.user_id == user.id)
        .all()
    )


@router.post("/", response_model=Budget)
def create_budget(
    budget: BudgetBase,
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> Budget:
    pass


@router.put("/{budget_id}", response_model=Budget)
def update_budget(
    budget_id: int,
    budget: BudgetBase,
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> Budget:
    pass

@router.delete("/{budget_id}", response_model=None)
def delete_budget(
    budget_id: int,
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> None:
    pass



@router.get("/{budget_id}/entries", response_model=list[BudgetEntryOut])
def get_budget_entries(
    budget_id: int,
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> list[BudgetEntry]:
    pass


@router.post("/{budget_id}/entries", response_model=BudgetEntryOut)
def create_budget_entry(
    budget_id: int,
    entry: BudgetEntryBase,
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> BudgetEntry:
    pass


@router.put("/entry/{entry_id}", response_model=BudgetEntryOut)
def update_budget_entry(
    entry_id: int,
    entry: BudgetEntryBase,
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> BudgetEntry:
    pass


@router.delete("/entry/{entry_id}", response_model=None)
def delete_budget_entry(
    entry_id: int,
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> None:
    pass



@router.get("/{budget_id}/categories", response_model=list[CategoryOut])
def get_budget_categories(
    budget_id: int,
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> list[BudgetCategoryLink]:
    pass


@router.post("/{budget_id}/categories", response_model=BudgetCategoryLinkOut)
def create_budget_category(
    budget_id: int,
    category: CategoryBase,
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> BudgetCategoryLink:
    pass


@router.put("/categories/{category_id}", response_model=BudgetCategoryLinkOut)
def update_budget_category(
    category_id: int,
    category: CategoryBase,
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> BudgetCategoryLink:
    pass


@router.delete("/categories/{category_id}", response_model=None)
def delete_budget_category(
    category_id: int,
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> None:
    pass