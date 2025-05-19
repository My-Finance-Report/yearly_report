from fastapi import APIRouter, Depends, HTTPException
from app.db import get_current_active_superuser
from app.models.user import User
from app.seed.accounts_page import delete_account_page, seed_account_page

router = APIRouter(tags=["admin"])

@router.post("/admin/reseed-account-page/{user_id}")
def reseed_account_page(
    user_id: int,
    current_user: User = Depends(get_current_active_superuser),
):
    if not current_user.is_superuser:
        raise HTTPException(status_code=403, detail="Admin access required")
        
    try:
        delete_account_page(user_id)
    except Exception as e:
        print(f"❌ Failed to delete account page: {e}")
    try:
        seed_account_page(user_id)
        return {"status": "success"}
    except Exception as e:
        print(f"❌ Failed to seed account page: {e}")
        raise HTTPException(status_code=500, detail=str(e))
