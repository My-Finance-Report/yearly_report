import logging
from datetime import datetime

from app.db import get_auth_db, get_db_for_user
from app.models import PlaidAccount, User
from app.plaid.sync_service import sync_all_plaid_accounts

logger = logging.getLogger(__name__)


async def sync_all_plaid_accounts_job():
    """Scheduled job to sync all Plaid accounts for all users."""
    logger.info(f"Starting scheduled Plaid sync job at {datetime.now()}")
    session = next(get_auth_db())
    
    try:
        # Get all users with Plaid accounts
        users_with_plaid = session.query(User).join(
            PlaidAccount, User.id == PlaidAccount.user_id
        ).distinct().all()
        
        
        for user in users_with_plaid:
            user_session = get_db_for_user(user.id)
            await sync_all_plaid_accounts(user_session, user, days_back=7)
            user_session.close()
        
       
    except Exception as e:
        logger.error(f"Error in Plaid sync job: {str(e)}")
    finally:
        session.close()


