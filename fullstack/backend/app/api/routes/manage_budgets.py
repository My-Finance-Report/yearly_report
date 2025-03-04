from collections import defaultdict

from fastapi import APIRouter, Depends, HTTPException
from sqlalchemy.orm import Session

from app.db import get_current_user, get_db
from app.local_types import (
    BudgetBase,
    BudgetCreate,
    BudgetCategoryLinkBase,
    BudgetCategoryLinkOut,
    BudgetEntryBase,
    BudgetEntryOut,
    BudgetOut,
    BudgetEntryCreate
)
from app.models import Budget, BudgetCategoryLink, BudgetEntry, BudgetEntryId, User

router = APIRouter(prefix="/budgets", tags=["budgets"])


def get_budget_out(session: Session, user: User) -> BudgetOut | None:
    budget=  (
        session.query(Budget)
        .filter(Budget.user_id == user.id)
        .first()
    )

    if not budget:
        return None

    entries = (
        session.query(BudgetEntry)
        .filter(BudgetEntry.budget_id == budget.id, BudgetEntry.user_id == user.id)
        .all()
    )

    entry_ids = [entry.id for entry in entries]
    categories = (
        session.query(BudgetCategoryLink)
        .filter(BudgetCategoryLink.budget_entry_id.in_(entry_ids), BudgetCategoryLink.user_id == user.id)
        .all()
    )

    category_by_entry: dict[BudgetEntryId, list[BudgetCategoryLinkOut]] = defaultdict(list)
    for category in categories:
        category_by_entry[category.budget_entry_id].append(BudgetCategoryLinkOut.model_validate(category.__dict__))

    entries_out = [
        BudgetEntryOut(
            **entry.__dict__,
            category_links=category_by_entry[entry.id],
        )
        for entry in entries
    ]
    return BudgetOut(
        **budget.__dict__,
        entries=entries_out,
    )


@router.get("/", response_model=BudgetOut | None)
def Get_budget(
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> BudgetOut | None:
    return get_budget_out(session=session, user=user)


@router.post("/", response_model=BudgetOut)
def create_budget(
    budget: BudgetCreate,
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> Budget:

    existing_budget = session.query(Budget).filter(
        Budget.name == budget.name, Budget.user_id == user.id, Budget.active
    ).first()
    if existing_budget:
        raise HTTPException(status_code=400, detail="Budget with this name already exists.")

    new_budget = Budget(**budget.model_dump(), user_id=user.id, active=True)
    session.add(new_budget)
    session.commit()
    session.refresh(new_budget)
    return new_budget


@router.put("/{budget_id}", response_model=BudgetOut)
def update_budget(
    budget_id: int,
    budget: BudgetBase,
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> Budget:
    db_budget = (
        session.query(Budget)
        .filter(Budget.id == budget_id, Budget.user_id == user.id, Budget.active)
        .one_or_none()
    )

    if not db_budget:
        raise HTTPException(status_code=404, detail="Budget not found.")

    for key, value in budget.model_dump().items():
        setattr(db_budget, key, value)

    session.commit()
    session.refresh(db_budget)
    return db_budget


@router.delete("/{budget_id}", response_model=None)
def delete_budget(
    budget_id: int,
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> None:
    db_budget = (
        session.query(Budget)
        .filter(Budget.id == budget_id, Budget.user_id == user.id)
        .one_or_none()
    )

    if not db_budget:
        raise HTTPException(status_code=404, detail="Budget not found.")

    db_budget.active = False
    session.commit()


@router.get("/{budget_id}/entries", response_model=list[BudgetEntryOut])
def get_budget_entries(
    budget_id: int,
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> list[BudgetEntryOut]:
    db_entries = (
        session.query(BudgetEntry)
        .filter(BudgetEntry.budget_id == budget_id, BudgetEntry.user_id == user.id)
        .all()
    )

    entry_ids = [entry.id for entry in db_entries]

    budget_links = (
        session.query(BudgetCategoryLink)
        .filter(
            BudgetCategoryLink.budget_entry_id.in_(entry_ids),
            BudgetCategoryLink.user_id == user.id,
        )
        .all()
    )

    link_by_entry = defaultdict(list)
    for link in budget_links:
        link_by_entry[link.budget_entry_id].append(link)

    if not db_entries:
        raise HTTPException(status_code=404, detail="Budget not found.")

    return [
        BudgetEntryOut.model_validate(
            {**entry.__dict__, "category_links": link_by_entry[entry.id]}
        )
        for entry in db_entries
    ]


@router.post("/{budget_id}/entries", response_model=BudgetEntryOut)
def create_budget_entry(
    budget_id: int,
    entry: BudgetEntryCreate,
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> BudgetEntryOut:
    new_entry = BudgetEntry(**entry.model_dump(), budget_id=budget_id, user_id=user.id)
    session.add(new_entry)
    session.commit()
    session.refresh(new_entry)
    return BudgetEntryOut.model_validate({**new_entry.__dict__, "category_links":[]})


@router.put("/entry/{entry_id}", response_model=BudgetEntryOut)
def update_budget_entry(
    entry_id: int,
    entry: BudgetEntryBase,
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> BudgetEntry:
    db_entry = (
        session.query(BudgetEntry)
        .filter(BudgetEntry.id == entry_id, BudgetEntry.user_id == user.id)
        .one_or_none()
    )

    if not db_entry:
        raise HTTPException(status_code=404, detail="Entry not found.")

    for key, value in entry.model_dump().items():
        setattr(db_entry, key, value)

    session.commit()
    session.refresh(db_entry)
    return db_entry


@router.delete("/entry/{entry_id}", response_model=None)
def delete_budget_entry(
    entry_id: int,
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> None:
    db_entry = (
        session.query(BudgetEntry)
        .filter(BudgetEntry.id == entry_id, BudgetEntry.user_id == user.id)
        .one_or_none()
    )

    if not db_entry:
        raise HTTPException(status_code=404, detail="Entry not found.")

    session.delete(db_entry)
    session.commit()
    return None


@router.get("/{budget_entry_id}/categories", response_model=list[BudgetCategoryLinkOut])
def get_budget_categories(
    budget_entry_id: int,
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> list[BudgetCategoryLink]:
    return (
        session.query(BudgetCategoryLink)
        .filter(
            BudgetCategoryLink.budget_entry_id == budget_entry_id,
            BudgetCategoryLink.user_id == user.id,
        )
        .all()
    )


@router.post("/{budget_entry_id}/categories", response_model=BudgetCategoryLinkOut)
def create_budget_category(
    budget_entry_id: int,
    category: BudgetCategoryLinkBase,
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> BudgetCategoryLink:
    new_link = BudgetCategoryLink(
        **category.model_dump(), budget_entry_id=budget_entry_id, user_id=user.id
    )
    session.add(new_link)
    session.commit()
    session.refresh(new_link)
    return new_link


@router.put("/categories/{category_link_id}", response_model=BudgetCategoryLinkOut)
def update_budget_category(
    category_link_id: int,
    category: BudgetCategoryLinkBase,
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> BudgetCategoryLink:
    db_link = (
        session.query(BudgetCategoryLink)
        .filter(
            BudgetCategoryLink.id == category_link_id,
            BudgetCategoryLink.user_id == user.id,
        )
        .one_or_none()
    )

    if not db_link:
        raise HTTPException(status_code=404, detail="Category link not found.")

    for key, value in category.model_dump().items():
        setattr(db_link, key, value)

    session.commit()
    session.refresh(db_link)
    return db_link


@router.delete("/categories/{category_link_id}", response_model=None)
def delete_budget_category(
    category_link_id: int,
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> None:
    db_link = (
        session.query(BudgetCategoryLink)
        .filter(
            BudgetCategoryLink.id == category_link_id,
            BudgetCategoryLink.user_id == user.id,
        )
        .one_or_none()
    )

    if not db_link:
        raise HTTPException(status_code=404, detail="Category link not found.")

    session.delete(db_link)
    session.commit()
    return None
