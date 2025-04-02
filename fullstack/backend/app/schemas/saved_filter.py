from datetime import datetime
from typing import Dict, Optional

from pydantic import BaseModel, Field


class SavedFilterBase(BaseModel):
    name: str
    description: Optional[str] = None
    filter_data: Dict
    is_public: bool = False


class SavedFilterCreate(SavedFilterBase):
    pass


class SavedFilterUpdate(BaseModel):
    name: Optional[str] = None
    description: Optional[str] = None
    filter_data: Optional[Dict] = None
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
