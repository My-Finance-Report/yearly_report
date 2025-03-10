# demo_data.py

import random
from dataclasses import dataclass
from datetime import datetime, timedelta

from faker import Faker

# Import your models/enums
from app.models import (
    Category,
    SourceKind,
    Transaction,
    TransactionKind,
    TransactionSource,
)


@dataclass
class DemoData:
    sources: list[TransactionSource]
    categories: list[Category]
    transactions: list[Transaction]


fake = Faker()

HARDCODED_CATEGORIES: dict[SourceKind, list[str]] = {
    SourceKind.account: [
        "Income",
        "Investments",
        "Credit Card Payments",
        "Transfers",
        "Housing",
    ],
    SourceKind.card: [
        "Groceries",
        "Travel",
        "Gas",
        "Insurance",
        "Misc",
        "Subscriptions",
        "Credit Card Payments",
        "Entertainment",
    ],
    SourceKind.investment: ["Stocks", "Bonds", "Index Funds"],
}
USE_HARDCODED = True


def generate_demo_data(num_transactions: int = 1000) -> DemoData:
    sources = []
    for index, bank in enumerate(
        ["Wells Fargo Checking", "Capital One Credit Card", "Wells Fargo Savings"]
    ):
        sources.append(
            TransactionSource(
                id=index + 1,
                name=bank,
                user_id=1,
                archived=False,
                source_kind=SourceKind.account if "Wells" in bank else SourceKind.card,
            )
        )

    categories = []
    for index, source in enumerate(sources):
        for category in HARDCODED_CATEGORIES[source.source_kind]:
            categories.append(
                Category(
                    id=index + 1,
                    name=category,
                    source_id=source.id,
                    user_id=1,
                    archived=False,
                )
            )

    # Generate transactions with random dates and amounts
    transactions = []
    start_date = datetime(2023, 1, 1)
    for i in range(1, num_transactions + 1):
        random_date = start_date + timedelta(days=random.randint(0, 365))
        transactions.append(
            Transaction(
                id=100 + i,
                description=fake.sentence(nb_words=4),
                category_id=random.choice(categories).id,
                date_of_transaction=random_date,
                amount=round(random.uniform(10, 1000), 2),
                transaction_source_id=random.choice(sources).id,
                kind=random.choice(
                    [TransactionKind.deposit, TransactionKind.withdrawal]
                ),
                user_id=1,
                archived=False,
            )
        )

    val = DemoData(sources, categories, transactions)
    return val


DATA = generate_demo_data()


def get_demo_data() -> DemoData:
    return DATA
