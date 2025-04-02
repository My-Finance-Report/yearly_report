from datetime import datetime
from typing import Optional

from pydantic import BaseModel

from app.models import FilterData


class SavedFilterBase(BaseModel):
    name: str
    description: Optional[str] = None
    filter_data: FilterData
    is_public: bool = False


class SavedFilterCreate(SavedFilterBase):
    pass


class SavedFilterUpdate(BaseModel):
    name: Optional[str] = None
    description: Optional[str] = None
    filter_data: Optional[FilterData] = None
    is_public: Optional[bool] = None


class SavedFilterInDB(SavedFilterBase):
    id: int
    user_id: int
    created_at: datetime
    updated_at: datetime

    class Config:
        orm_mode = True


class SavedFilter(SavedFilterInDB):
    pass
