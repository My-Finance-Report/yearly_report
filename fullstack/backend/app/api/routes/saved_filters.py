import uuid
from datetime import datetime

from fastapi import APIRouter, Depends, HTTPException
from sqlalchemy.orm import Session

from app.db import get_current_user, get_db
from app.models import (
    FilterData,
    FilterEntries,
    FilterEntry,
    GroupByOption,
    SavedFilter,
    User,
)
from app.schemas.saved_filter import (
    SavedFilterCreate,
    SavedFilterUpdate,
)
from app.schemas.saved_filter import (
    SavedFilterOut as SavedFilterSchema,
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
    )


def make_default_filters() -> list[SavedFilterSchema]:
    return [
        SavedFilterSchema(
            id=str(uuid.uuid4()),
            is_deleteable=False,
            name="Year To Date",
            description="All categories, by month for last year",
            filter_data=FilterData(
                is_default=True,
                lookup={
                    GroupByOption.year: FilterEntries(
                        visible=False,
                        specifics=[FilterEntry(value=str(datetime.now().year))],
                        index=1,
                    ),
                    GroupByOption.month: FilterEntries(
                        visible=True, specifics=None, index=1
                    ),
                    GroupByOption.category: FilterEntries(
                        specifics=None, visible=True, index=0
                    ),
                },
            ),
        ),
        SavedFilterSchema(
            id=str(uuid.uuid4()),
            is_deleteable=False,
            name="Last Year",
            description="All categories, by month for last year",
            filter_data=FilterData(
                is_default=True,
                lookup={
                    GroupByOption.year: FilterEntries(
                        visible=False,
                        specifics=[FilterEntry(value=str(datetime.now().year))],
                        index=2,
                    ),
                    GroupByOption.month: FilterEntries(
                        visible=True, specifics=None, index=1
                    ),
                    GroupByOption.category: FilterEntries(
                        specifics=None, visible=True, index=0
                    ),
                },
            ),
        ),
        SavedFilterSchema(
            id=str(uuid.uuid4()),
            is_deleteable=False,
            name="All Time",
            description="All categories, by month for all time",
            filter_data=FilterData(
                is_default=True,
                lookup={
                    GroupByOption.year: FilterEntries(
                        visible=True, specifics=None, index=2
                    ),
                    GroupByOption.month: FilterEntries(
                        visible=True, specifics=None, index=1
                    ),
                    GroupByOption.category: FilterEntries(
                        specifics=None, visible=True, index=0
                    ),
                },
            ),
        ),
        SavedFilterSchema(
            id=str(uuid.uuid4()),
            is_deleteable=False,
            name="Monthly Budget",
            description="All categories, by month for current year",
            filter_data=FilterData(
                is_default=True,
                lookup={
                    GroupByOption.budget: FilterEntries(
                        specifics=None, visible=True, index=0
                    ),
                    GroupByOption.year: FilterEntries(
                        visible=False,
                        specifics=[FilterEntry(value=str(datetime.now().year))],
                        index=2,
                    ),
                    GroupByOption.month: FilterEntries(
                        visible=True,
                        specifics=[FilterEntry(value=str(datetime.now().month))],
                        index=1,
                    ),
                },
            ),
        ),
    ]


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
    return make_default_filters() + [
        SavedFilterSchema(
            id=f.id,
            name=f.name,
            description=f.description,
            filter_data=f.filter_data,
        )
        for f in filters
    ]


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
    )


@router.delete("/{filter_id}", response_model=None)
def delete_saved_filter(
    *,
    db: Session = Depends(get_db),
    filter_id: int,
    current_user: User = Depends(get_current_user),
) -> None:
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
    return None
