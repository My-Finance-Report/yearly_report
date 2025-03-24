import logging
from datetime import datetime

from app.db import get_auth_db, get_db_for_user
from app.models import User
from app.plaid.sync_service import sync_all_plaid_accounts

logger = logging.getLogger(__name__)


async def sync_all_plaid_accounts_job() -> None:
    """Scheduled job to sync all Plaid accounts for all users."""
    logger.info(f"Starting scheduled Plaid sync job at {datetime.now()}")
    session = next(get_auth_db())

    print("trying to pull users")
    users = session.query(User).all()

    for user in users:
        user_session = next(get_db_for_user(user.id))
        await sync_all_plaid_accounts(user_session, user, days_back=7)
        user_session.close()

    session.close()
