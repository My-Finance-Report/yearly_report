# demo_data.py

from datetime import datetime

# Import your models/enums
from app.models import (
    Transaction,
    Category,
    TransactionSource,
    TransactionKind,
    SourceKind,
)
# If your models live in a different file, adjust imports accordingly.

def get_demo_data():
    """Return lists of mock data for demonstration and filtering."""
    
    # Mock transaction sources
    sources = [
        TransactionSource(
            id=1,
            name="Main Checking",
            user_id=1,
            archived=False,
            source_kind=SourceKind.account,
        ),
        TransactionSource(
            id=2,
            name="Credit Card",
            user_id=1,
            archived=False,
            source_kind=SourceKind.account,
        ),
        TransactionSource(
            id=3,
            name="Employer Payroll",
            user_id=1,
            archived=False,
            source_kind=SourceKind.account,
        )
    ]

    # Mock categories
    categories = [
        Category(
            id=1, 
            name="Groceries", 
            source_id=1,  # belongs to Main Checking
            user_id=1,
            archived=False,
        ),
        Category(
            id=2, 
            name="Rent",
            source_id=1,  
            user_id=1,
            archived=False,
        ),
        Category(
            id=3, 
            name="Utilities",
            source_id=2,  # belongs to Credit Card
            user_id=1,
            archived=False,
        ),
        Category(
            id=4, 
            name="Salary",
            source_id=3,  # belongs to Employer
            user_id=1,
            archived=False,
        )
    ]

    # Mock transactions
    transactions = [
        Transaction(
            id=101,
            description="Grocery store purchase",
            category_id=1,
            date_of_transaction=datetime(2023, 1, 5),
            amount=150,
            transaction_source_id=1,  # from Main Checking
            kind=TransactionKind.withdrawal,
            user_id=1,
            archived=False,
        ),
        Transaction(
            id=102,
            description="Monthly rent payment",
            category_id=2,
            date_of_transaction=datetime(2023, 1, 1),
            amount=1200,
            transaction_source_id=1,
            kind=TransactionKind.withdrawal,
            user_id=1,
            archived=False,
        ),
        Transaction(
            id=103,
            description="Power bill",
            category_id=3,
            date_of_transaction=datetime(2023, 2, 15),
            amount=75,
            transaction_source_id=2,  # from Credit Card
            kind=TransactionKind.withdrawal,
            user_id=1,
            archived=False,
        ),
        Transaction(
            id=104,
            description="Salary (Jan)",
            category_id=4,
            date_of_transaction=datetime(2023, 1, 31),
            amount=3000,
            transaction_source_id=3,  # from Employer
            kind=TransactionKind.deposit,
            user_id=1,
            archived=False,
        ),
        Transaction(
            id=105,
            description="Grocery store purchase",
            category_id=1,
            date_of_transaction=datetime(2023, 2, 2),
            amount=180,
            transaction_source_id=1,
            kind=TransactionKind.withdrawal,
            user_id=1,
            archived=False,
        ),
        Transaction(
            id=106,
            description="Salary (Feb)",
            category_id=4,
            date_of_transaction=datetime(2023, 2, 28),
            amount=3000,
            transaction_source_id=3,
            kind=TransactionKind.deposit,
            user_id=1,
            archived=False,
        ),
        Transaction(
            id=107,
            description="Restaurant dinner",
            category_id=1, 
            date_of_transaction=datetime(2024, 1, 10),
            amount=60,
            transaction_source_id=2,
            kind=TransactionKind.withdrawal,
            user_id=1,
            archived=False,
        ),
    ]

    return {
        "sources": sources,
        "categories": categories,
        "transactions": transactions,
    }
