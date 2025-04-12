
from fastapi import APIRouter, Depends
from sqlalchemy.orm import Session

from app.db import get_current_user, get_db
from app.models import  User
from app.local_types import WorkerStatusOut
from app.worker.status import get_latest_batch

router = APIRouter(prefix="/worker-status", tags=["worker-status"])

@router.post("/status", response_model=list[WorkerStatusOut])
def get_status(
    *,
    db: Session = Depends(get_db),
    current_user: User = Depends(get_current_user),
) ->list[WorkerStatusOut]:
 
    outputs = get_latest_batch(session=db, user=current_user)
    return [WorkerStatusOut(id=row.id, batch_id=row.batch_id, status=row.status, created_at=row.created_at, updated_at=row.updated_at, additional_info=row.additional_info) for row in outputs]