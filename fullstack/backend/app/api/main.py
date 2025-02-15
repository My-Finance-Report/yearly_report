from fastapi import APIRouter

from app.api.routes import login, private, users, utils, uploads, manage_accounts
from app.core.config import settings
from app.api.routes import transactions

api_router = APIRouter()
api_router.include_router(login.router)
api_router.include_router(transactions.router)
api_router.include_router(manage_accounts.router)
api_router.include_router(uploads.router)
api_router.include_router(users.router)
api_router.include_router(utils.router)


if settings.ENVIRONMENT == "local":
    api_router.include_router(private.router)
