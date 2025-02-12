from fastapi import APIRouter, Depends

from app.api.deps import (
    get_current_user,
)
from app.db import Session, get_db
from app.models import (
    Transaction,
    User,
)

from app.local_types import (
    TransactionOut
)

router = APIRouter(prefix="/transactions", tags=["transactions"])


@router.get(
    "/",
    dependencies=[Depends(get_current_user)],
    response_model=list[TransactionOut],
)
def get_transactions(session:Session =Depends(get_db), user: User = Depends(get_current_user)) -> list[Transaction]:
    val =  session.query(Transaction).filter(Transaction.user_id == user.id).all()
    return val
    


