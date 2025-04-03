import datetime
from fastapi import APIRouter, Depends, HTTPException
from sqlalchemy.orm import Session

from app.db import get_current_user, get_db
from app.models import FilterData, SavedFilter, User
from app.schemas.saved_filter import (
    SavedFilter as SavedFilterSchema,
)
from app.schemas.saved_filter import (
    SavedFilterCreate,
    SavedFilterUpdate,
)

router = APIRouter(prefix="/saved-filters", tags=["saved-filters"])


@router.post("/", response_model=SavedFilterSchema)
def create_saved_filter(
    *,
    db: Session = Depends(get_db),
    filter_in: SavedFilterCreate,
    current_user: User = Depends(get_current_user),
) -> SavedFilterSchema:
    """
    Create a new saved filter.
    """
    saved_filter = SavedFilter(
        user_id=current_user.id,
        name=filter_in.name,
        description=filter_in.description,
        filter_data=filter_in.filter_data,
    )
    db.add(saved_filter)
    db.commit()
    db.refresh(saved_filter)
    return SavedFilterSchema(
        id=saved_filter.id,
        name=saved_filter.name,
        description=saved_filter.description,
        filter_data=saved_filter.filter_data,
        created_at=saved_filter.created_at,
        updated_at=saved_filter.updated_at,
        user_id=saved_filter.user_id,
    )


@router.get("/", response_model=list[SavedFilterSchema])
def read_saved_filters(
    *,
    db: Session = Depends(get_db),
    current_user: User = Depends(get_current_user),
    skip: int = 0,
    limit: int = 100,
) -> list[SavedFilterSchema]:
    """
    Retrieve saved filters.
    """
    filters = (
        db.query(SavedFilter)
        .filter(SavedFilter.user_id == current_user.id)
        .offset(skip)
        .limit(limit)
        .all()
    )
    return [
        SavedFilterSchema(
            id=f.id,
            name=f.name,
            description=f.description,
            filter_data=f.filter_data,
            created_at=f.created_at,
            updated_at=f.updated_at,
            user_id=f.user_id,
        )
        for f in filters
    ]


@router.get("/public", response_model=list[SavedFilterSchema])
def read_public_saved_filters(
    *,
    _db: Session = Depends(get_db),
    current_user: User = Depends(get_current_user),
    _skip: int = 0,
    _limit: int = 100,
) -> list[SavedFilterSchema]:
    """
    Retrieve public saved filters from all users.
    """
    # todo
    return [SavedFilterSchema(
        id=1,
        name="Default Filter",
        description="Default filter description",
        filter_data=FilterData(),
        created_at=datetime.datetime.now(),
        updated_at=datetime.datetime.now(),
        user_id=current_user.id,
    )]


@router.get("/{filter_id}", response_model=SavedFilterSchema)
def read_saved_filter(
    *,
    db: Session = Depends(get_db),
    filter_id: int,
    current_user: User = Depends(get_current_user),
) -> SavedFilterSchema:
    """
    Get a specific saved filter by ID.
    """
    saved_filter = (
        db.query(SavedFilter)
        .filter(SavedFilter.id == filter_id, SavedFilter.user_id == current_user.id)
        .first()
    )
    if not saved_filter:
        raise HTTPException(status_code=404, detail="Saved filter not found")

    return SavedFilterSchema(
        id=saved_filter.id,
        name=saved_filter.name,
        description=saved_filter.description,
        filter_data=saved_filter.filter_data,
        created_at=saved_filter.created_at,
        updated_at=saved_filter.updated_at,
        user_id=saved_filter.user_id,
    )


@router.get("/by-name/{filter_name}", response_model=SavedFilterSchema)
def read_saved_filter_by_name(
    *,
    db: Session = Depends(get_db),
    filter_name: str,
    current_user: User = Depends(get_current_user),
) -> SavedFilterSchema:
    """
    Get a specific saved filter by name.
    """
    saved_filter = (
        db.query(SavedFilter)
        .filter(SavedFilter.name == filter_name)
        .filter(SavedFilter.user_id == current_user.id)
        .first()
    )
    if not saved_filter:
        raise HTTPException(status_code=404, detail="Saved filter not found")

    return SavedFilterSchema(
        id=saved_filter.id,
        name=saved_filter.name,
        description=saved_filter.description,
        filter_data=saved_filter.filter_data,
        created_at=saved_filter.created_at,
        updated_at=saved_filter.updated_at,
        user_id=saved_filter.user_id,
    )


@router.put("/{filter_id}", response_model=SavedFilterSchema)
def update_saved_filter(
    *,
    db: Session = Depends(get_db),
    filter_id: int,
    filter_in: SavedFilterUpdate,
    current_user: User = Depends(get_current_user),
) -> SavedFilterSchema:
    """
    Update a saved filter.
    """
    saved_filter = db.query(SavedFilter).filter(SavedFilter.id == filter_id).first()
    if not saved_filter:
        raise HTTPException(status_code=404, detail="Saved filter not found")

    # Check if the user owns this filter
    if saved_filter.user_id != current_user.id:
        raise HTTPException(status_code=403, detail="Not enough permissions")

    update_data = filter_in.model_dump(exclude_unset=True)
    for field, value in update_data.items():
        setattr(saved_filter, field, value)

    db.add(saved_filter)
    db.commit()
    db.refresh(saved_filter)
    return SavedFilterSchema(
        id=saved_filter.id,
        name=saved_filter.name,
        description=saved_filter.description,
        filter_data=saved_filter.filter_data,
        created_at=saved_filter.created_at,
        updated_at=saved_filter.updated_at,
        user_id=saved_filter.user_id,
    )


@router.delete("/{filter_id}", response_model=SavedFilterSchema)
def delete_saved_filter(
    *,
    db: Session = Depends(get_db),
    filter_id: int,
    current_user: User = Depends(get_current_user),
) -> SavedFilterSchema:
    """
    Delete a saved filter.
    """
    saved_filter = db.query(SavedFilter).filter(SavedFilter.id == filter_id).first()
    if not saved_filter:
        raise HTTPException(status_code=404, detail="Saved filter not found")

    # Check if the user owns this filter
    if saved_filter.user_id != current_user.id:
        raise HTTPException(status_code=403, detail="Not enough permissions")

    db.delete(saved_filter)
    db.commit()
    return SavedFilterSchema(
        id=saved_filter.id,
        name=saved_filter.name,
        description=saved_filter.description,
        filter_data=saved_filter.filter_data,
        created_at=saved_filter.created_at,
        updated_at=saved_filter.updated_at,
        user_id=saved_filter.user_id,
    )
