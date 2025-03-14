from fastapi import APIRouter, Depends, HTTPException
from sqlalchemy.orm import Session

from app.api.routes.manage_budgets import get_stylized_name_lookup
from app.db import get_current_user, get_db
from app.local_types import (
    CategoryBase,
    CategoryOut,
    TransactionSourceBase,
    TransactionSourceOut,
)
from app.models import Category, Transaction, TransactionSource, User
from app.worker.enqueue_job import enqueue_recategorization

router = APIRouter(prefix="/accounts", tags=["accounts"])


@router.get("/", response_model=list[TransactionSourceOut])
def get_transaction_sources(
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> list[TransactionSourceOut]:
    has_transactions = (
        session.query(Transaction.transaction_source_id)
        .filter(Transaction.user_id == user.id)
        .distinct()
        .all()
    )

    source_ids_with_transactions = [t.transaction_source_id for t in has_transactions]

    db_sources = (
        session.query(TransactionSource)
        .filter(
            TransactionSource.id.in_(source_ids_with_transactions),
            TransactionSource.user_id == user.id,
        )
        .all()
    )

    return [
        TransactionSourceOut(
            name=db_source.name, archived=db_source.archived, id=db_source.id
        )
        for db_source in db_sources
    ]


@router.post("/", response_model=TransactionSourceOut)
def create_transaction_source(
    transaction_source: TransactionSourceBase,
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> TransactionSourceOut:
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

    return TransactionSourceOut(
        name=new_source.name, archived=new_source.archived, id=new_source.id
    )


@router.put("/{source_id}", response_model=TransactionSourceOut)
def update_transaction_source(
    source_id: int,
    transaction_source: TransactionSourceBase,
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> TransactionSourceOut:
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
    return TransactionSourceOut(
        name=db_source.name, archived=db_source.archived, id=db_source.id
    )


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
) -> list[CategoryOut]:
    source = (
        session.query(TransactionSource)
        .filter(TransactionSource.id == source_id, TransactionSource.user_id == user.id)
        .first()
    )

    if not source:
        raise HTTPException(status_code=404, detail="Transaction source not found.")

    db_categories = (
        session.query(Category).filter(Category.source_id == source_id).all()
    )

    stylized_name_lookup = get_stylized_name_lookup(session, user)

    return [
        CategoryOut(
            name=db_category.name,
            archived=db_category.archived,
            id=db_category.id,
            source_id=db_category.source_id,
            stylized_name=stylized_name_lookup[db_category.id],
        )
        for db_category in db_categories
    ]


@router.post("/{source_id}/categories", response_model=CategoryOut)
def create_category(
    source_id: int,
    category: CategoryBase,
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> CategoryOut:
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

    stylized_name_lookup = get_stylized_name_lookup(session, user)

    return CategoryOut(
        name=new_category.name,
        archived=new_category.archived,
        id=new_category.id,
        source_id=new_category.source_id,
        stylized_name=stylized_name_lookup[new_category.id],
    )


@router.put("/categories/{category_id}", response_model=CategoryOut)
def update_category(
    category_id: int,
    category: CategoryBase,
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> CategoryOut:
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
    stylized_name_lookup = get_stylized_name_lookup(session, user)
    return CategoryOut(
        name=db_category.name,
        archived=db_category.archived,
        id=db_category.id,
        source_id=db_category.source_id,
        stylized_name=stylized_name_lookup[db_category.id],
    )


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
