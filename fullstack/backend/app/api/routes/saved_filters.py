from typing import List

from fastapi import APIRouter, Depends, HTTPException
from sqlalchemy.orm import Session

from app.db import get_current_user, get_db
from app.models import SavedFilter, User
from app.schemas.saved_filter import (
    SavedFilter as SavedFilterSchema,
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
):
    """
    Create a new saved filter.
    """
    saved_filter = SavedFilter(
        user_id=current_user.id,
        name=filter_in.name,
        description=filter_in.description,
        filter_data=filter_in.filter_data,
        is_public=filter_in.is_public,
    )
    db.add(saved_filter)
    db.commit()
    db.refresh(saved_filter)
    return saved_filter


@router.get("/", response_model=List[SavedFilterSchema])
def read_saved_filters(
    *,
    db: Session = Depends(get_db),
    current_user: User = Depends(get_current_user),
    skip: int = 0,
    limit: int = 100,
):
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
    return filters


@router.get("/public", response_model=List[SavedFilterSchema])
def read_public_saved_filters(
    *,
    db: Session = Depends(get_db),
    current_user: User = Depends(get_current_user),
    skip: int = 0,
    limit: int = 100,
):
    """
    Retrieve public saved filters from all users.
    """
    #todo
    return []


@router.get("/{filter_id}", response_model=SavedFilterSchema)
def read_saved_filter(
    *,
    db: Session = Depends(get_db),
    filter_id: int,
    current_user: User = Depends(get_current_user),
):
    """
    Get a specific saved filter by ID.
    """
    saved_filter = db.query(SavedFilter).filter(SavedFilter.id == filter_id).first()
    if not saved_filter:
        raise HTTPException(status_code=404, detail="Saved filter not found")
    
    # Check if the user has access to this filter
    if saved_filter.user_id != current_user.id and not saved_filter.is_public:
        raise HTTPException(status_code=403, detail="Not enough permissions")
    
    return saved_filter


@router.get("/by-name/{filter_name}", response_model=SavedFilterSchema)
def read_saved_filter_by_name(
    *,
    db: Session = Depends(get_db),
    filter_name: str,
    current_user: User = Depends(get_current_user),
):
    """
    Get a specific saved filter by name.
    """
    saved_filter = (
        db.query(SavedFilter)
        .filter(SavedFilter.name == filter_name)
        .filter(
            (SavedFilter.user_id == current_user.id) | (SavedFilter.is_public == True)
        )
        .first()
    )
    if not saved_filter:
        raise HTTPException(status_code=404, detail="Saved filter not found")
    
    return saved_filter


@router.put("/{filter_id}", response_model=SavedFilterSchema)
def update_saved_filter(
    *,
    db: Session = Depends(get_db),
    filter_id: int,
    filter_in: SavedFilterUpdate,
    current_user: User = Depends(get_current_user),
):
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
    return saved_filter


@router.delete("/{filter_id}", response_model=SavedFilterSchema)
def delete_saved_filter(
    *,
    db: Session = Depends(get_db),
    filter_id: int,
    current_user: User = Depends(get_current_user),
):
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
    return saved_filter
