import logging
from datetime import datetime, timedelta
from typing import Any, Dict, List, Optional

from plaid.api.plaid_api import TransactionsGetRequest
from plaid.model.transactions_get_request_options import TransactionsGetRequestOptions
from sqlalchemy.orm import Session

from app.models import (
    Category,
    PlaidAccount,
    PlaidItem,
    PlaidSyncLog,
    ProcessFileJob,
    Transaction,
    TransactionKind,
    TransactionSource,
    User,
)
from app.plaid.client import get_plaid_client
from app.async_pipelines.uploaded_file_pipeline.configuration_creator import add_default_categories
from app.async_pipelines.recategorize_pipeline.main import apply_previous_recategorizations
from app.async_pipelines.uploaded_file_pipeline.categorizer import categorize_extracted_transactions, insert_categorized_transactions
from app.async_pipelines.uploaded_file_pipeline.local_types import InProcessFile, PartialTransaction, TransactionsWrapper
from app.func_utils import pipe

logger = logging.getLogger(__name__)


def get_transaction_kind(amount: float) -> TransactionKind:
    """Determine transaction kind based on amount.
    In Plaid, positive amounts are outflows (withdrawals), negative are inflows (deposits)
    """
    return TransactionKind.withdrawal if amount >= 0 else TransactionKind.deposit


def fetch_plaid_transactions(
    access_token: str, 
    start_date: datetime.date, 
    end_date: datetime.date,
    account_ids: Optional[List[str]] = None
) -> List[Any]:
    """Fetch transactions from Plaid API."""
    client = get_plaid_client()
    
    options = None
    if account_ids:
        options = TransactionsGetRequestOptions(account_ids=account_ids)
    
    request = TransactionsGetRequest(
        access_token=access_token,
        start_date=start_date,
        end_date=end_date,
        options=options
    )
    
    try:
        response = client.transactions_get(request)
        return response["transactions"]
    except Exception as e:
        logger.error(f"Error fetching transactions from Plaid: {str(e)}")
        raise


def has_transaction_changed(local_transaction: Transaction, plaid_transaction: Any) -> bool:
    """Check if a transaction has changed in Plaid compared to our local copy."""
    # Convert Plaid amount to match our format
    plaid_amount = abs(float(plaid_transaction["amount"]))
    
    # Check if any important fields have changed
    if abs(local_transaction.amount - plaid_amount) > 0.001:
        return True
    
    if local_transaction.description != plaid_transaction["name"]:
        return True
    
    local_date = local_transaction.date_of_transaction.date()
    plaid_date = plaid_transaction["date"]
    if local_date != plaid_date:
        return True
    
    return False


def sync_plaid_account_transactions(
    session: Session,
    user: User,
    plaid_account: PlaidAccount,
    days_back: int = 30
) -> None: 
    plaid_item = session.query(PlaidItem).filter(
        PlaidItem.id == plaid_account.plaid_item_id
    ).one()
    
    # Get the transaction source for this account
    transaction_source = session.query(TransactionSource).filter(
        TransactionSource.plaid_account_id == plaid_account.id
    ).one()

    # Set date range for transaction fetch
    end_date = datetime.now().date()
    start_date = end_date - timedelta(days=days_back)
    
    # Create a sync log entry
    sync_log = PlaidSyncLog(
        user_id=user.id,
        plaid_item_id=plaid_item.id,
        plaid_account_id=plaid_account.id,
        sync_type="incremental",
        start_date=start_date,
        end_date=end_date,
    )
    session.add(sync_log)
    
    try:
        # Fetch transactions from Plaid
        plaid_transactions = fetch_plaid_transactions(
            plaid_item.access_token,
            start_date,
            end_date,
            [plaid_account.plaid_account_id]
        )
        
        # Get existing transactions for this account
        existing_transactions = session.query(Transaction).filter(
            Transaction.user_id == user.id,
            Transaction.transaction_source_id == transaction_source.id,
            Transaction.external_id.is_not(None),
            Transaction.archived == False
        ).all()
        
        # Create lookup dictionaries
        existing_by_external_id = {t.external_id: t for t in existing_transactions if t.external_id}
        plaid_by_transaction_id = {pt["transaction_id"]: pt for pt in plaid_transactions}
        
        # Find new transactions
        new_transaction_ids = set(plaid_by_transaction_id.keys()) - set(existing_by_external_id.keys())
        
        # Find removed transactions
        removed_transaction_ids = set(existing_by_external_id.keys()) - set(plaid_by_transaction_id.keys())
        
        # Find potentially modified transactions
        common_transaction_ids = set(plaid_by_transaction_id.keys()) & set(existing_by_external_id.keys())
        modified_transaction_ids = {
            tid for tid in common_transaction_ids 
            if has_transaction_changed(existing_by_external_id[tid], plaid_by_transaction_id[tid])
        }
        
        # Process new transactions
        added_count = 0
        if new_transaction_ids:
            added_count = add_new_transactions(
                session, 
                user, 
                transaction_source, 
                [plaid_by_transaction_id[tid] for tid in new_transaction_ids]
            )
        
        # Process modified transactions
        modified_count = 0
        if modified_transaction_ids:
            modified_count = update_modified_transactions(
                session,
                [plaid_by_transaction_id[tid] for tid in modified_transaction_ids],
                {tid: existing_by_external_id[tid] for tid in modified_transaction_ids}
            )
        
        # Process removed transactions
        removed_count = 0
        if removed_transaction_ids:
            removed_count = archive_removed_transactions(
                session,
                [existing_by_external_id[tid] for tid in removed_transaction_ids]
            )
        
        # Update sync log with results
        sync_log.added_count = added_count
        sync_log.modified_count = modified_count
        sync_log.removed_count = removed_count
        
        session.commit()
        
    except Exception as e:
        logger.error(f"Error syncing Plaid account {plaid_account.id}: {str(e)}")
        sync_log.error_message = str(e)
        session.commit()
        raise


def plaid_categorize_pipe(in_process: InProcessFile)->None: 
    print("categorizing")
    return pipe(
    in_process,
    #apply_previous_recategorizations, TODO reapply
    categorize_extracted_transactions,
    final=insert_categorized_transactions,
    )



def add_new_transactions(
    session: Session,
    user: User,
    transaction_source: TransactionSource,
    plaid_transactions: List[Any]
) -> int:
    categories = session.query(Category).filter(
        Category.source_id == transaction_source.id,
        Category.user_id == user.id,
        Category.archived == False
    ).all()

    if not categories:
        add_default_categories(session, user, transaction_source)
        categories = session.query(Category).filter(
            Category.source_id == transaction_source.id,
            Category.user_id == user.id,
            Category.archived == False
        ).all()

    in_process = InProcessFile(
        session=session,
        user=user,
        transaction_source=transaction_source,
        categories=categories,
        transactions=TransactionsWrapper(
            transactions=[
                PartialTransaction(
                    partialTransactionId=None,
                    partialTransactionAmount=abs(float(pt["amount"])),
                    partialTransactionDescription=pt["name"],
                    partialTransactionDateOfTransaction=pt["date"].strftime("%m/%d/%Y"),
                    partialTransactionKind=get_transaction_kind(float(pt["amount"])).value,
                )
                for pt in plaid_transactions
            ]
        ),
    )
   
    plaid_categorize_pipe(in_process)
    assert in_process.transactions, "must have"
    return len(in_process.transactions.transactions)

def update_modified_transactions(
    session: Session,
    plaid_transactions: List[Any],
    local_transactions: Dict[str, Transaction]
) -> int:
    """Update local transactions that have changed in Plaid."""
    count = 0
    for pt in plaid_transactions:
        local_tx = local_transactions.get(pt["transaction_id"])
        if not local_tx:
            continue
            
        local_tx.description = pt["name"]
        local_tx.date_of_transaction = pt["date"]
        local_tx.amount = abs(float(pt["amount"]))
        local_tx.kind = get_transaction_kind(float(pt["amount"]))
        local_tx.last_updated = datetime.now()
        
        session.add(local_tx)
        count += 1
    
    session.flush()
    return count


def archive_removed_transactions(
    session: Session,
    transactions: List[Transaction]
) -> int:
    """Archive transactions that no longer exist in Plaid."""
    for transaction in transactions:
        transaction.archived = True
        transaction.last_updated = datetime.now()
        session.add(transaction)
    
    session.flush()
    return len(transactions)


async def sync_all_plaid_accounts(user_session: Session, user: User, days_back: int = 30) -> None:
    """
    Sync all Plaid accounts for a specific user.
    """
    
    try:
        plaid_accounts = user_session.query(PlaidAccount).filter(
            PlaidAccount.user_id == user.id
        ).all()
        
        for account in plaid_accounts:
            sync_plaid_account_transactions(
                user_session, user, account, days_back
            )

        print(f"Synced all Plaid accounts for user {user.id}")
            
    
    except Exception as e:
        logger.error(f"Error syncing Plaid accounts for user {user.id}: {str(e)}")
        raise
