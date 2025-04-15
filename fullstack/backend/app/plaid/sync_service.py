import logging
import uuid
from dataclasses import replace
from datetime import datetime, timedelta
from typing import Any

from plaid.api.plaid_api import TransactionsSyncRequest, TransactionsSyncResponse
from plaid.model.transactions_sync_request_options import TransactionsSyncRequestOptions
from sqlalchemy.orm import Session

from app.async_pipelines.uploaded_file_pipeline.categorizer import (
    categorize_extracted_transactions,
)
from app.async_pipelines.uploaded_file_pipeline.configuration_creator import (
    add_default_categories,
)
from app.async_pipelines.uploaded_file_pipeline.local_types import (
    InProcessJob,
    PartialTransaction,
    Recategorization,
    TransactionsWrapper,
)
from app.func_utils import pipe
from app.models import (
    AuditLog,
    Category,
    PlaidAccount,
    PlaidItem,
    PlaidSyncLog,
    ProcessingState,
    Transaction,
    TransactionKind,
    TransactionSource,
    User,
)
from app.plaid.client import get_plaid_client
from app.telegram_utils import send_telegram_message
from app.worker.status import status_update_monad, update_worker_status

logger = logging.getLogger(__name__)


def get_transaction_kind(amount: float) -> TransactionKind:
    """Determine transaction kind based on amount.
    In Plaid, positive amounts are outflows (withdrawals), negative are inflows (deposits)
    """
    return TransactionKind.withdrawal if amount >= 0 else TransactionKind.deposit


def fetch_plaid_transactions(
    *,
    access_token: str,
    plaid_account: PlaidAccount,
) -> Any:
    """Fetch transactions from Plaid API."""
    client = get_plaid_client()

    request = TransactionsSyncRequest(
        access_token=access_token,
        cursor=plaid_account.cursor or "",
        count=100,
        options=TransactionsSyncRequestOptions(
            account_id=plaid_account.plaid_account_id
        ),
    )

    try:
        response: TransactionsSyncResponse = client.transactions_sync(request)
        next_cursor = response["next_cursor"]
        plaid_account.cursor = next_cursor

        return response["added"]
    except Exception as e:
        logger.error(f"Error fetching transactions from Plaid: {str(e)}")
        raise


def has_transaction_changed(
    local_transaction: Transaction, plaid_transaction: Any
) -> bool:
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
    days_back: int,
    batch_id: str,
) -> None:
    plaid_item = (
        session.query(PlaidItem)
        .filter(PlaidItem.id == plaid_account.plaid_item_id)
        .one()
    )

    # Get the transaction source for this account
    transaction_source = (
        session.query(TransactionSource)
        .filter(TransactionSource.plaid_account_id == plaid_account.id)
        .one()
    )

    # Set date range for transaction fetch (not directly used by transactions_sync but kept for logging)
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
            access_token=plaid_item.access_token,
            plaid_account=plaid_account,
        )

        # If no transactions were returned, we're done
        if not plaid_transactions:
            sync_log.added_count = 0
            sync_log.modified_count = 0
            sync_log.removed_count = 0
            session.commit()
            return

        # All transactions from transactions_sync are new

        added_count = add_new_transactions(
            session,
            user,
            transaction_source,
            plaid_transactions,
        )

        # Update sync log with results
        sync_log.added_count = added_count
        sync_log.modified_count = 0  # For transactions_sync, we don't track modified
        sync_log.removed_count = 0  # For transactions_sync, we don't track removed

        session.commit()
        update_worker_status(
            session,
            user,
            status=ProcessingState.completed,
            additional_info="Completed Plaid sync",
            batch_id=batch_id,
        )

    except Exception as e:
        logger.error(f"Error syncing Plaid account {plaid_account.id}: {str(e)}")
        sync_log.error_message = str(e)
        session.commit()
        raise


def plaid_categorize_pipe(in_process: InProcessJob) -> None:
    return pipe(
        in_process,
        lambda x: status_update_monad(
            x,
            status=ProcessingState.categorizing_transactions,
            additional_info="Applying previous recategorizations",
        ),
        apply_previous_plaid_recategorizations,
        lambda x: status_update_monad(
            x,
            status=ProcessingState.categorizing_transactions,
            additional_info="Categorizing batches",
        ),
        categorize_extracted_transactions,
        lambda x: status_update_monad(
            x,
            status=ProcessingState.categorizing_transactions,
            additional_info="Writing batching to database",
        ),
        final=insert_categorized_plaid_transactions,
    )


def apply_previous_plaid_recategorizations(in_process: InProcessJob) -> InProcessJob:
    assert in_process.transaction_source, "must have"
    assert in_process.transactions, "must have"
    assert in_process.categories, "must have"
    query = (
        in_process.session.query(AuditLog, Transaction)
        .join(Transaction, Transaction.id == AuditLog.transaction_id)
        .filter(
            Transaction.transaction_source_id == in_process.transaction_source.id,
            Transaction.user_id == in_process.user.id,
            Transaction.external_id.in_(
                [t.partialTransactionId for t in in_process.transactions.transactions]
            ),
            ~Transaction.archived,
            AuditLog.apply_to_future,
        )
        .all()
    )

    category_lookup = {cat.id: cat.name for cat in in_process.categories}

    recats: list[Recategorization] = []
    auditlog: AuditLog
    transaction: Transaction
    for auditlog, transaction in query:
        if auditlog.change.new_kind and auditlog.change.old_kind:
            recats.append(
                Recategorization(
                    description=transaction.description,
                    previous_category=auditlog.change.old_kind,
                    overrided_category=auditlog.change.new_kind,
                )
            )
        if auditlog.change.new_category and auditlog.change.old_category:
            recats.append(
                Recategorization(
                    description=transaction.description,
                    previous_category=category_lookup[auditlog.change.old_category],
                    overrided_category=category_lookup[auditlog.change.new_category],
                )
            )

    return replace(
        in_process,
        previous_recategorizations=recats or None,
    )


def insert_categorized_plaid_transactions(in_process: InProcessJob) -> None:
    assert in_process.transaction_source, "must have"
    assert in_process.categorized_transactions, "must have"
    assert in_process.categories, "must have"
    category_lookup = {cat.name: cat.id for cat in in_process.categories}

    transactions_to_insert = [
        Transaction(
            description=t.partialTransactionDescription,
            category_id=category_lookup[t.category],
            date_of_transaction=datetime.strptime(
                t.partialTransactionDateOfTransaction, "%m/%d/%Y"
            ),
            amount=t.partialTransactionAmount,
            transaction_source_id=in_process.transaction_source.id,
            kind=t.partialTransactionKind,
            external_id=t.partialTransactionId,
            uploaded_pdf_id=in_process.file.id if in_process.file else None,
            user_id=in_process.user.id,
            archived=False,
        )
        for t in in_process.categorized_transactions
    ]

    in_process.session.bulk_save_objects(transactions_to_insert)
    in_process.session.commit()


def add_new_transactions(
    session: Session,
    user: User,
    transaction_source: TransactionSource,
    plaid_transactions: list[Any],
) -> int:
    categories = (
        session.query(Category)
        .filter(
            Category.source_id == transaction_source.id,
            Category.user_id == user.id,
            ~Category.archived,
        )
        .all()
    )

    if not categories:
        add_default_categories(session, user, transaction_source)
        categories = (
            session.query(Category)
            .filter(
                Category.source_id == transaction_source.id,
                Category.user_id == user.id,
                ~Category.archived,
            )
            .all()
        )

    in_process = InProcessJob(
        session=session,
        user=user,
        batch_id=uuid.uuid4().hex,
        transaction_source=transaction_source,
        categories=categories,
        transactions=TransactionsWrapper(
            transactions=[
                PartialTransaction(
                    partialTransactionId=None,
                    partialPlaidTransactionId=pt["transaction_id"],
                    partialTransactionAmount=abs(float(pt["amount"])),
                    partialTransactionDescription=pt["name"],
                    partialTransactionDateOfTransaction=pt["date"].strftime("%m/%d/%Y"),
                    partialTransactionKind=get_transaction_kind(
                        float(pt["amount"])
                    ).value,
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
    plaid_transactions: list[Any],
    local_transactions: dict[str, Transaction],
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
    session: Session, transactions: list[Transaction]
) -> int:
    """Archive transactions that no longer exist in Plaid."""
    for transaction in transactions:
        transaction.archived = True
        transaction.last_updated = datetime.now()
        session.add(transaction)

    session.flush()
    return len(transactions)


async def sync_all_plaid_accounts(
    user_session: Session, user: User, days_back: int
) -> None:
    """
    Sync all Plaid accounts for a specific user.
    """

    try:
        plaid_accounts = (
            user_session.query(PlaidAccount)
            .filter(PlaidAccount.user_id == user.id)
            .all()
        )

        for account in plaid_accounts:
            batch_id = uuid.uuid4().hex
            sync_plaid_account_transactions(
                user_session, user, account, days_back, batch_id
            )

        if plaid_accounts:
            print(f"Synced all Plaid accounts for user {user.id}")
        else:
            print(f"No Plaid accounts found for user {user.id}")

    except Exception as e:
        send_telegram_message(
            f"Error syncing Plaid accounts for user {user.id}: {str(e)}"
        )
        logger.error(f"Error syncing Plaid accounts for user {user.id}: {str(e)}")
        raise
