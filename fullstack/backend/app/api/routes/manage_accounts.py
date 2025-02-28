from fastapi import APIRouter, Depends, HTTPException
from sqlalchemy.orm import Session

from app.api.deps import get_current_user
from app.db import get_db
from app.local_types import (
    CategoryBase,
    CategoryOut,
    TransactionSourceBase,
    TransactionSourceOut,
)
from app.models import Category, TransactionSource, User
from app.worker.enqueue_job import enqueue_recategorization

router = APIRouter(prefix="/accounts", tags=["accounts"])


@router.get("/", response_model=list[TransactionSourceOut])
def get_transaction_sources(
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> list[TransactionSource]:
    return (
        session.query(TransactionSource)
        .filter(TransactionSource.user_id == user.id)
        .all()
    )


@router.post("/", response_model=TransactionSourceOut)
def create_transaction_source(
    transaction_source: TransactionSourceBase,
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> TransactionSource:
    existing_source = (
        session.query(TransactionSource)
        .filter(
            TransactionSource.user_id == user.id,
            TransactionSource.name == transaction_source.name,
        )
        .one()
    )
    if existing_source:
        raise HTTPException(
            status_code=400, detail="An account with this name already exists."
        )

    new_source = TransactionSource(**transaction_source.model_dump(), user_id=user.id)
    session.add(new_source)
    session.commit()
    session.refresh(new_source)

    return new_source


@router.put("/{source_id}", response_model=TransactionSourceOut)
def update_transaction_source(
    source_id: int,
    transaction_source: TransactionSourceBase,
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> TransactionSource:
    db_source = (
        session.query(TransactionSource)
        .filter(TransactionSource.id == source_id, TransactionSource.user_id == user.id)
        .first()
    )

    if not db_source:
        raise HTTPException(status_code=404, detail="Transaction source not found.")

    for key, value in transaction_source.dict().items():
        setattr(db_source, key, value)

    session.commit()
    session.refresh(db_source)
    return db_source


@router.delete("/{source_id}", response_model=None)
def delete_transaction_source(
    source_id: int,
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> None:
    db_source = (
        session.query(TransactionSource)
        .filter(TransactionSource.id == source_id, TransactionSource.user_id == user.id)
        .first()
    )

    if not db_source:
        raise HTTPException(status_code=404, detail="Transaction source not found.")

    session.delete(db_source)
    session.commit()


@router.get("/{source_id}/categories", response_model=list[CategoryOut])
def get_categories(
    source_id: int,
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> list[Category]:
    source = (
        session.query(TransactionSource)
        .filter(TransactionSource.id == source_id, TransactionSource.user_id == user.id)
        .first()
    )

    if not source:
        raise HTTPException(status_code=404, detail="Transaction source not found.")

    return session.query(Category).filter(Category.source_id == source_id).all()


@router.post("/{source_id}/categories", response_model=CategoryOut)
def create_category(
    source_id: int,
    category: CategoryBase,
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> Category:
    source = (
        session.query(TransactionSource)
        .filter(TransactionSource.id == source_id, TransactionSource.user_id == user.id)
        .first()
    )

    if not source:
        raise HTTPException(status_code=404, detail="Transaction source not found.")

    existing_category = (
        session.query(Category)
        .filter(Category.source_id == source_id, Category.name == category.name)
        .first()
    )

    if existing_category:
        raise HTTPException(
            status_code=400, detail="Category with this name already exists."
        )

    new_category = Category(**category.model_dump(), user_id=user.id)
    session.add(new_category)
    session.commit()
    session.refresh(new_category)

    enqueue_recategorization(
        session=session, user_id=user.id, transaction_source_id=new_category.source_id
    )

    return new_category


@router.put("/categories/{category_id}", response_model=CategoryOut)
def update_category(
    category_id: int,
    category: CategoryBase,
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> Category:
    db_category = (
        session.query(Category)
        .filter(Category.id == category_id, Category.user_id == user.id)
        .first()
    )

    if not db_category:
        raise HTTPException(status_code=404, detail="Category not found.")

    for key, value in category.model_dump().items():
        setattr(db_category, key, value)

    session.commit()
    session.refresh(db_category)
    return db_category


@router.delete("/categories/{category_id}", response_model=None)
def delete_category(
    category_id: int,
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> None:
    db_category = (
        session.query(Category)
        .filter(Category.id == category_id, Category.user_id == user.id)
        .first()
    )

    if not db_category:
        raise HTTPException(status_code=404, detail="Category not found.")

    session.delete(db_category)
    session.commit()

    enqueue_recategorization(
        session=session, user_id=user.id, transaction_source_id=db_category.source_id
    )
