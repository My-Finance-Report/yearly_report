from datetime import datetime, timezone
from sqlalchemy.orm import Session
from app.models import ProcessingState, User, WorkerStatus
from app.async_pipelines.uploaded_file_pipeline.local_types import InProcessJob


def get_latest_batch(session: Session, user: User)-> list[WorkerStatus]:
    latest_status = session.query(WorkerStatus).filter(WorkerStatus.user_id == user.id).order_by(WorkerStatus.created_at.desc()).first()
    if latest_status is None:
        return []
    batch_id = latest_status.batch_id
    return get_batch_status(session, user, batch_id)

def get_batch_status(session:Session, user: User, batch_id:str) -> list[WorkerStatus]: 
    return [row for row in session.query(WorkerStatus).filter(WorkerStatus.user_id == user.id, WorkerStatus.batch_id == batch_id)]

def update_worker_status(session:Session, user: User, status: ProcessingState, additional_info: str, batch_id: str)-> WorkerStatus: 
    new_status = WorkerStatus(user_id = user.id,batch_id=batch_id, status=status, additional_info=additional_info, created_at=datetime.now(timezone.utc), updated_at=datetime.now(timezone.utc))
    session.add(new_status)
    session.commit()

    return new_status

def status_update_monad(in_process: InProcessJob, status: ProcessingState, additional_info: str)-> InProcessJob:
    '''is this actually a monad?'''
    update_worker_status(in_process.session, in_process.user, status=status, additional_info=additional_info, batch_id=in_process.batch_id)
    return in_process

