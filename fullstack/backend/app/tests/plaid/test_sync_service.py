import uuid
from datetime import datetime, timedelta
from unittest.mock import MagicMock, patch

import pytest
from sqlalchemy.orm import Session

from app.async_pipelines.uploaded_file_pipeline.local_types import (
    CategorizedTransaction,
    InProcessJob,
    PartialTransaction,
    TransactionsWrapper,
)
from app.models.transaction import Transaction, TransactionKind, PlaidTransactionId
from app.models.transaction_source import TransactionSource
from app.models.category import (
    Category,
)
from app.models.plaid import PlaidAccount, PlaidItem, PlaidSyncLog
from app.models.user import User
from app.models.worker_status import ProcessingState
from app.plaid.sync_service import (
    PlaidFetchResponse,
    fetch_existing_plaid_transactions,
    fetch_plaid_transactions,
    insert_categorized_plaid_transactions,
)
from app.tests.utils.utils import TestKit, random_lower_string


@patch('app.plaid.sync_service.get_plaid_client')
def test_fetch_plaid_transactions_set_next_cursor(mock_get_plaid_client):
    mock_client = MagicMock()
    mock_get_plaid_client.return_value = mock_client
    
    mock_response = {
        "added": [{"id": "tx1", "amount": 100}],
        "modified": [],
        "removed": [],
        "accounts": [{"id": "acc1", "name": "Checking"}],
        "next_cursor": "next_cursor_value"
    }
    mock_client.transactions_sync.return_value = mock_response
    
    plaid_account = PlaidAccount()
    plaid_account.cursor = "initial_cursor"
    plaid_account.plaid_account_id = "acc1"
    
    result = fetch_plaid_transactions(
        access_token="test_token",
        plaid_account=plaid_account
    )
    
    assert isinstance(result, PlaidFetchResponse)
    assert result.added == mock_response["added"]
    assert result.removed == mock_response["removed"]
    assert result.modified == mock_response["modified"]
    assert result.accounts == mock_response["accounts"]
    
    assert plaid_account.cursor == "initial_cursor"
    
    result.set_next_cursor()
    assert plaid_account.cursor == "next_cursor_value"

def test_fetch_existing_plaid_transactions(test_kit: TestKit):
    session = test_kit.session
    user = test_kit.user
    # Create transaction source
    transaction_source = TransactionSource(
        name=random_lower_string(),
        user_id=user.id,
        source_kind="account",
    )

    session.add(transaction_source)

    session.flush()

    # Create categories
    category1 = Category(
        name="Category 1",
        source_id=transaction_source.id,
        user_id=user.id,
    )
    category2 = Category(
        name="Category 2",
        source_id=transaction_source.id,
        user_id=user.id,
    )
    session.add(category1)
    session.add(category2)
    session.flush()

    # Create existing transactions
    external_id1 = f"plaid_tx_{uuid.uuid4().hex}"
    external_id2 = f"plaid_tx_{uuid.uuid4().hex}"

    tx1 = Transaction(
        description="Transaction 1",
        category_id=category1.id,
        date_of_transaction=datetime.now(),
        amount=100.0,
        transaction_source_id=transaction_source.id,
        kind=TransactionKind.withdrawal,
        external_id=external_id1,
        user_id=user.id,
    )
    tx2 = Transaction(
        description="Transaction 2",
        category_id=category2.id,
        date_of_transaction=datetime.now(),
        amount=200.0,
        transaction_source_id=transaction_source.id,
        kind=TransactionKind.deposit,
        external_id=external_id2,
        user_id=user.id,
    )
    session.add(tx1)
    session.add(tx2)
    session.commit()

    # Create InProcessJob with transactions that match the existing ones
    in_process = InProcessJob(
        session=session,
        user=user,
        batch_id=uuid.uuid4().hex,
        transaction_source=transaction_source,
        categories=[category1, category2],
        transactions=TransactionsWrapper(
            transactions=[
                PartialTransaction(
                    partialTransactionId=None,
                    partialPlaidTransactionId=PlaidTransactionId(external_id1),
                    partialTransactionAmount=100.0,
                    partialTransactionDescription="Transaction 1",
                    partialTransactionDateOfTransaction=datetime.now().strftime(
                        "%m/%d/%Y"
                    ),
                    partialTransactionKind=TransactionKind.withdrawal.value,
                ),
                PartialTransaction(
                    partialTransactionId=None,
                    partialPlaidTransactionId=PlaidTransactionId(external_id2),
                    partialTransactionAmount=200.0,
                    partialTransactionDescription="Transaction 2",
                    partialTransactionDateOfTransaction=datetime.now().strftime(
                        "%m/%d/%Y"
                    ),
                    partialTransactionKind=TransactionKind.deposit.value,
                ),
                # New transaction that doesn't exist yet
                PartialTransaction(
                    partialTransactionId=None,
                    partialPlaidTransactionId=PlaidTransactionId(
                        f"plaid_tx_{uuid.uuid4().hex}"
                    ),
                    partialTransactionAmount=300.0,
                    partialTransactionDescription="Transaction 3",
                    partialTransactionDateOfTransaction=datetime.now().strftime(
                        "%m/%d/%Y"
                    ),
                    partialTransactionKind=TransactionKind.withdrawal.value,
                ),
            ]
        ),
    )

    # Run the function
    result = fetch_existing_plaid_transactions(in_process)

    # Verify results
    assert result.existing_transactions is not None
    assert len(result.existing_transactions) == 2

    # Check that the existing transactions were found
    external_ids = [tx.external_id for tx in result.existing_transactions]
    assert external_id1 in external_ids
    assert external_id2 in external_ids


def test_insert_categorized_plaid_transactions_update_existing(test_kit: TestKit):
    session = test_kit.session
    user = test_kit.user

    # Create transaction source
    transaction_source = TransactionSource(
        name=random_lower_string(),
        user_id=user.id,
        source_kind="account",
    )
    session.add(transaction_source)
    session.flush()

    # Create categories
    category1 = Category(
        name="Category 1",
        source_id=transaction_source.id,
        user_id=user.id,
    )
    category2 = Category(
        name="Category 2",
        source_id=transaction_source.id,
        user_id=user.id,
    )
    session.add(category1)
    session.add(category2)
    session.flush()

    # Create an existing transaction
    external_id = f"plaid_tx_{uuid.uuid4().hex}"

    old_date = datetime.now() - timedelta(days=5)
    tx = Transaction(
        description="Old Description",
        category_id=category1.id,
        date_of_transaction=old_date,
        amount=100.0,
        transaction_source_id=transaction_source.id,
        kind=TransactionKind.withdrawal,
        external_id=external_id,
        user_id=user.id,
    )
    session.add(tx)
    session.commit()

    # Create InProcessJob with updated transaction data
    new_date = datetime.now().strftime("%m/%d/%Y")
    new_amount = 150.0
    new_description = "Updated Description"

    in_process = InProcessJob(
        session=session,
        user=user,
        batch_id=uuid.uuid4().hex,
        transaction_source=transaction_source,
        categories=[category1, category2],
        existing_transactions=[tx],
        categorized_transactions=[
            CategorizedTransaction(
                partialTransactionId=None,
                partialPlaidTransactionId=PlaidTransactionId(external_id),
                partialTransactionAmount=new_amount,
                partialTransactionDescription=new_description,
                partialTransactionDateOfTransaction=new_date,
                partialTransactionKind=TransactionKind.withdrawal.value,
                category="Category 2",  # Changed category
            ),
        ],
    )

    # Run the function
    result = insert_categorized_plaid_transactions(in_process)

    # Refresh the transaction from the database
    session.refresh(tx)

    # Verify the transaction was updated
    assert tx.description == new_description
    assert tx.amount == new_amount
    assert tx.category_id == category2.id  # Should be updated to category2
    assert tx.date_of_transaction.strftime("%m/%d/%Y") == new_date

    # Verify no new transactions were inserted
    assert result.inserted_transactions is not None
    assert len(result.inserted_transactions) == 0


def test_insert_categorized_plaid_transactions_insert_new(test_kit: TestKit):
    session = test_kit.session
    user = test_kit.user

    # Create transaction source
    transaction_source = TransactionSource(
        name=random_lower_string(),
        user_id=user.id,
        source_kind="account",
    )
    session.add(transaction_source)
    session.flush()

    # Create categories
    category1 = Category(
        name="Category 1",
        source_id=transaction_source.id,
        user_id=user.id,
    )
    session.add(category1)
    session.flush()

    # Create InProcessJob with a new transaction
    external_id = f"plaid_tx_{uuid.uuid4().hex}"
    transaction_date = datetime.now().strftime("%m/%d/%Y")

    in_process = InProcessJob(
        session=session,
        user=user,
        batch_id=uuid.uuid4().hex,
        transaction_source=transaction_source,
        categories=[category1],
        existing_transactions=[],  # No existing transactions
        categorized_transactions=[
            CategorizedTransaction(
                partialTransactionId=None,
                partialPlaidTransactionId=PlaidTransactionId(external_id),
                partialTransactionAmount=200.0,
                partialTransactionDescription="New Transaction",
                partialTransactionDateOfTransaction=transaction_date,
                partialTransactionKind=TransactionKind.withdrawal.value,
                category="Category 1",
            ),
        ],
    )

    # Run the function
    result = insert_categorized_plaid_transactions(in_process)

    # Verify a new transaction was inserted
    assert result.inserted_transactions is not None
    assert len(result.inserted_transactions) == 1

    # Query the database to verify the transaction was actually saved
    new_tx = (
        session.query(Transaction)
        .filter(Transaction.external_id == external_id)
        .first()
    )
    assert new_tx is not None
    assert new_tx.description == "New Transaction"
    assert new_tx.amount == 200.0
    assert new_tx.category_id == category1.id
    assert new_tx.date_of_transaction.strftime("%m/%d/%Y") == transaction_date


def test_insert_categorized_plaid_transactions_mixed(test_kit: TestKit):
    session = test_kit.session
    user = test_kit.user

    # Create transaction source
    transaction_source = TransactionSource(
        name=random_lower_string(),
        user_id=user.id,
        source_kind="account",
    )
    session.add(transaction_source)
    session.flush()

    # Create categories
    category1 = Category(
        name="Category 1",
        source_id=transaction_source.id,
        user_id=user.id,
    )
    category2 = Category(
        name="Category 2",
        source_id=transaction_source.id,
        user_id=user.id,
    )
    session.add(category1)
    session.add(category2)
    session.flush()

    # Create an existing transaction
    existing_external_id = f"plaid_tx_{uuid.uuid4().hex}"

    existing_tx = Transaction(
        description="Existing Transaction",
        category_id=category1.id,
        date_of_transaction=datetime.now() - timedelta(days=3),
        amount=100.0,
        transaction_source_id=transaction_source.id,
        kind=TransactionKind.withdrawal,
        external_id=existing_external_id,
        user_id=user.id,
    )
    session.add(existing_tx)
    session.commit()

    # Create InProcessJob with both existing and new transactions
    new_external_id = f"plaid_tx_{uuid.uuid4().hex}"
    transaction_date = datetime.now().strftime("%m/%d/%Y")

    in_process = InProcessJob(
        session=session,
        user=user,
        batch_id=uuid.uuid4().hex,
        transaction_source=transaction_source,
        categories=[category1, category2],
        existing_transactions=[existing_tx],
        categorized_transactions=[
            # Updated existing transaction
            CategorizedTransaction(
                partialTransactionId=None,
                partialPlaidTransactionId=PlaidTransactionId(existing_external_id),
                partialTransactionAmount=120.0,  # Updated amount
                partialTransactionDescription="Updated Transaction",  # Updated description
                partialTransactionDateOfTransaction=transaction_date,
                partialTransactionKind=TransactionKind.withdrawal.value,
                category="Category 2",  # Changed category
            ),
            # New transaction
            CategorizedTransaction(
                partialTransactionId=None,
                partialPlaidTransactionId=PlaidTransactionId(new_external_id),
                partialTransactionAmount=200.0,
                partialTransactionDescription="New Transaction",
                partialTransactionDateOfTransaction=transaction_date,
                partialTransactionKind=TransactionKind.deposit.value,
                category="Category 1",
            ),
        ],
    )

    # Run the function
    result = insert_categorized_plaid_transactions(in_process)

    # Refresh the existing transaction from the database
    session.refresh(existing_tx)

    # Verify the existing transaction was updated
    assert existing_tx.description == "Updated Transaction"
    assert existing_tx.amount == 120.0
    assert existing_tx.category_id == category2.id

    # Verify a new transaction was inserted
    assert result.inserted_transactions is not None
    assert len(result.inserted_transactions) == 1

    # Query the database to verify the new transaction was actually saved
    new_tx = (
        session.query(Transaction)
        .filter(Transaction.external_id == new_external_id)
        .first()
    )
    assert new_tx is not None
    assert new_tx.description == "New Transaction"
    assert new_tx.amount == 200.0
    assert new_tx.kind == TransactionKind.deposit
    assert new_tx.category_id == category1.id
