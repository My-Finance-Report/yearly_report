from datetime import datetime

from pydantic import BaseModel

from app.models import FilterData


class SavedFilterBase(BaseModel):
    name: str
    description: str | None = None
    filter_data: FilterData


class SavedFilterCreate(SavedFilterBase):
    pass


class SavedFilterUpdate(BaseModel):
    name: str | None = None
    description: str | None = None
    filter_data: FilterData | None = None


class SavedFilterInDB(SavedFilterBase):
    id: int
    user_id: int
    created_at: datetime
    updated_at: datetime

    class Config:
        orm_mode = True


class SavedFilterOut(SavedFilterBase):
    id: int | str  # uuid for defaults
    is_deleteable: bool = True
    is_default: bool = False
    pass
