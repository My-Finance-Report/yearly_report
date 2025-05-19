from fastapi import APIRouter

from app.api.routes import (
    admin,
    demo,
    login,
    notifications,
    manage_accounts,
    manage_budgets,
    no_code,
    oauth,
    plaid,
    private,
    sankey,
    saved_filters,
    sitemap,
    status,
    subscription,
    transactions,
    two_factor,
    uploads,
    users,
    utils,
)
from app.core.config import settings

api_router = APIRouter()
api_router.include_router(login.router)
api_router.include_router(admin.router)
api_router.include_router(notifications.router)
api_router.include_router(oauth.router)
api_router.include_router(plaid.router)
api_router.include_router(sitemap.router)
api_router.include_router(demo.router)
api_router.include_router(transactions.router)
api_router.include_router(manage_accounts.router)
api_router.include_router(manage_budgets.router)
api_router.include_router(no_code.router)
api_router.include_router(sankey.router)
api_router.include_router(uploads.router)
api_router.include_router(users.router)
api_router.include_router(utils.router)
api_router.include_router(subscription.router)
api_router.include_router(saved_filters.router)
api_router.include_router(two_factor.router)
api_router.include_router(status.router)

if settings.ENVIRONMENT == "local":
    api_router.include_router(private.router)
