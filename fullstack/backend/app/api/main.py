from fastapi import APIRouter

from app.api.routes import (
    demo,
    login,
    manage_accounts,
    manage_budgets,
    private,
    sankey,
    transactions,
    uploads,
    users,
    utils,
)
from app.core.config import settings

api_router = APIRouter()
api_router.include_router(login.router)
api_router.include_router(demo.router)
api_router.include_router(transactions.router)
api_router.include_router(manage_accounts.router)
api_router.include_router(manage_budgets.router)
api_router.include_router(sankey.router)
api_router.include_router(uploads.router)
api_router.include_router(users.router)
api_router.include_router(utils.router)


if settings.ENVIRONMENT == "local":
    api_router.include_router(private.router)
