from fastapi import APIRouter, Depends, HTTPException
from datetime import datetime, timezone
from sqlalchemy.orm import Session

from app.api.routes.manage_budgets import get_stylized_name_lookup
from app.db import get_current_user, get_db
from app.local_types import (
    CategoryOut,
    PlaidSyncLogOut,
    BalanceUpdate,
    TransactionSourceOut,
)
from app.models import (
    Category,
    CategoryBase,
    PlaidAccount,
    PlaidAccountBalance,
    PlaidSyncLog,
    PlaidSyncLogId,
    Transaction,
    TransactionSource,
    TransactionSourceBase,
    TransactionSourceId,
    User,
)
from app.worker.enqueue_job import enqueue_recategorization

router = APIRouter(prefix="/accounts", tags=["accounts"])


@router.get("/", response_model=list[TransactionSourceOut])
def get_transaction_sources(
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> list[TransactionSourceOut]:
    db_sources = (
        session.query(TransactionSource)
        .filter(
            TransactionSource.user_id == user.id,
        )
        .order_by(TransactionSource.id.asc())
        .all()
    )

    return [
        TransactionSourceOut(
            name=db_source.name,
            archived=db_source.archived,
            id=db_source.id,
            source_kind=db_source.source_kind,
            is_plaid_connected=db_source.plaid_account_id is not None,
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
        name=new_source.name,
        archived=new_source.archived,
        id=new_source.id,
        source_kind=new_source.source_kind,
        is_plaid_connected=new_source.plaid_account_id is not None,
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
        name=db_source.name,
        archived=db_source.archived,
        id=db_source.id,
        source_kind=db_source.source_kind,
        is_plaid_connected=db_source.plaid_account_id is not None,
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


@router.post("/merge-accounts", response_model=None)
def merge_accounts(
    to_keep_id: TransactionSourceId,
    to_merge_id: TransactionSourceId,
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> None:
    db_to_keep = (
        session.query(TransactionSource)
        .filter(
            TransactionSource.id == to_keep_id, TransactionSource.user_id == user.id
        )
        .one()
    )

    if not db_to_keep:
        raise HTTPException(status_code=404, detail="Transaction source not found.")

    db_to_merge = (
        session.query(TransactionSource)
        .filter(
            TransactionSource.id == to_merge_id, TransactionSource.user_id == user.id
        )
        .one()
    )

    if not db_to_merge:
        raise HTTPException(status_code=404, detail="Transaction source not found.")

    categories_in_account_we_loose = (
        session.query(Category).filter(Category.source_id == to_merge_id).all()
    )
    categories_in_account_we_keep = (
        session.query(Category).filter(Category.source_id == to_keep_id).all()
    )

    lookup_in_loose_by_id = {c.id: c for c in categories_in_account_we_loose}
    lookup_in_keep_by_name = {c.name.lower(): c for c in categories_in_account_we_keep}

    for transaction in session.query(Transaction).filter(
        Transaction.transaction_source_id == to_merge_id
    ):
        transaction.transaction_source_id = to_keep_id
        old_category = lookup_in_loose_by_id[transaction.category_id]
        if old_category.name.lower() not in lookup_in_keep_by_name:
            new_category = Category(
                name=old_category.name, user_id=user.id, source_id=to_keep_id
            )
            session.add(new_category)
            session.commit()
            session.refresh(new_category)
        transaction.category_id = lookup_in_keep_by_name[old_category.name].id

    session.commit()

    session.delete(db_to_merge)
    session.commit()


@router.post("/{source_id}/recategorize", response_model=dict)
def trigger_recategorization(
    source_id: int,
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> dict[str, str]:
    """Trigger recategorization for all transactions in a specific transaction source."""
    source = (
        session.query(TransactionSource)
        .filter(TransactionSource.id == source_id, TransactionSource.user_id == user.id)
        .one()
    )

    if not source:
        raise HTTPException(status_code=404, detail="Transaction source not found.")

    enqueue_recategorization(
        session=session, user_id=user.id, transaction_source_id=source_id
    )

    return {"status": "success", "message": "Recategorization job has been queued"}


@router.post("/{source_id}/toggle-archive", response_model=TransactionSourceOut)
def toggle_archive_transaction_source(
    source_id: int,
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> TransactionSourceOut:
    """Toggle the archive status of a transaction source."""
    db_source = (
        session.query(TransactionSource)
        .filter(TransactionSource.id == source_id, TransactionSource.user_id == user.id)
        .one()
    )

    if not db_source:
        raise HTTPException(status_code=404, detail="Transaction source not found.")

    # Toggle the archived status
    db_source.archived = not db_source.archived

    session.commit()
    session.refresh(db_source)
    return TransactionSourceOut(
        name=db_source.name,
        archived=db_source.archived,
        id=db_source.id,
        source_kind=db_source.source_kind,
        is_plaid_connected=db_source.plaid_account_id is not None,
    )


@router.get("/{source_id}/sync-logs", response_model=list[PlaidSyncLogOut])
def get_account_sync_logs(
    source_id: int,
    limit: int = 10,
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> list[PlaidSyncLogOut]:
    """Retrieve the most recent sync logs for a Plaid-connected account."""
    db_source = (
        session.query(TransactionSource)
        .filter(TransactionSource.id == source_id, TransactionSource.user_id == user.id)
        .first()
    )

    if not db_source:
        raise HTTPException(status_code=404, detail="Transaction source not found.")

    if not db_source.plaid_account_id:
        raise HTTPException(
            status_code=400, detail="This account is not connected to Plaid."
        )

    sync_logs = (
        session.query(PlaidSyncLog)
        .filter(
            PlaidSyncLog.user_id == user.id,
            PlaidSyncLog.plaid_account_id == db_source.plaid_account_id,
        )
        .order_by(PlaidSyncLog.created_at.desc())
        .limit(limit)
        .all()
    )

    return [
        PlaidSyncLogOut(
            id=PlaidSyncLogId(log.id),
            sync_type=log.sync_type,
            start_date=log.start_date,
            end_date=log.end_date,
            added_count=log.added_count,
            modified_count=log.modified_count,
            removed_count=log.removed_count,
            error_message=log.error_message,
            created_at=log.created_at,
        )
        for log in sync_logs
    ]




@router.put("/{source_id}", response_model=None)
def update_account_balance(
    source_id: int,
    balance_update: BalanceUpdate,
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> None:
    db_source = (
        session.query(TransactionSource)
        .filter(TransactionSource.id == source_id, TransactionSource.user_id == user.id)
        .first()
    )


    plaid_account = session.query(PlaidAccount.id).filter(PlaidAccount.transaction_source_id== db_source.id).one_or_none()


    plaid_account_id =plaid_account.id if plaid_account else None


    if not db_source:
        raise HTTPException(status_code=404, detail="Transaction source not found.")


    if user.id != db_source.user_id:
        raise HTTPException(status_code=403, detail="You cannot edit this")


    new_balance = PlaidAccountBalance(
        plaid_account_id = plaid_account_id,
        transation_source_id=source_id,
        balance=balance_update.balance,
        timestamp=balance_update.timestamp.astimezone(timezone.utc),
        )

    session.add(new_balance)
    session.commit()
    return None

