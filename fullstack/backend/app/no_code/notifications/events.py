from abc import ABC
from typing import NewType
from pydantic import BaseModel
from app.schemas.no_code import NoCodeBudgetEntry, NoCodeTransaction
from app.models.effect import EventType


Row = NewType("Row", str)
Table = NewType("Table", str)


def generate_budget_row(budget_entry: NoCodeBudgetEntry) -> Row:
    return generate_row(
        [
            budget_entry.category_name,
            f"${budget_entry.monthly_target:.2f}",
            f"${budget_entry.current_monthly_total:.2f}",
            f"{budget_entry.progress * 100:.0f}%",
        ]
    )


def generate_transaction_row(transaction: NoCodeTransaction) -> Row:
    amount_str = f"${abs(transaction.amount):.2f}"
    if transaction.kind == "withdrawal":
        amount_str = f"-{amount_str}"
    else:
        amount_str = f"+{amount_str}"

    date_str = transaction.date_of_transaction.strftime("%b %d")

    formatted_desc = transaction.description
    if len(formatted_desc) > 10:
        formatted_desc = formatted_desc[:10] + "..."

    return generate_row(
        [
            date_str,
            formatted_desc,
            transaction.category_name or "Uncategorized",
            amount_str,
        ]
    )


def generate_table(headers: list[str], rows: list[Row], suffix: str) -> Table:
    return Table(f"""<table>
        <thead>
            <tr>
                {"".join(f'<th style="padding: 8px; text-align: left;">{header}</th>' for header in headers)}
            </tr>
        </thead>
        <tbody>
            {"".join(rows)}
            {suffix}
        </tbody>
    </table>""")


def generate_row(entries: list[str]) -> Row:
    return Row(f"""<tr>
        {"".join(f"<td>{entry}</td>" for entry in entries)}
    </tr>""")


class Event(ABC, BaseModel):
    type: EventType

    @property
    def alter_settings(self) -> str:
        return "Click <a style='text-decoration: underline;' href='https://myfinancereport.com/notifications'>here</a> to change your notification settings"

    @property
    def transactions_table(self) -> Table:
        """Generate an HTML table of transactions."""
        if not hasattr(self, "transactions"):
            raise NotImplementedError(
                "must have transactions to build a transaction table"
            )

        transactions = getattr(self, "transactions")
        transactions_to_show = transactions[:5]
        has_more = len(transactions) > 5

        rows = [generate_transaction_row(tx) for tx in transactions_to_show]

        more_text = (
            f"<tr><td colspan='4'><em>...and {len(transactions) - 5} more transaction(s)</em></td></tr>"
            if has_more
            else ""
        )
        return generate_table(
            headers=["Date", "Desc.", "Category", "Amount"],
            rows=rows,
            suffix=more_text,
        )


class NewTransactionsEvent(Event):
    type: EventType = EventType.NEW_TRANSACTION
    transactions: list[NoCodeTransaction]
    account_name: str
    count: int


class NewAccountLinkedEvent(Event):
    type: EventType = EventType.NEW_ACCOUNT_LINKED
    account_name: str


class AccountDeactivatedEvent(Event):
    type: EventType = EventType.ACCOUNT_DEACTIVATED
    account_name: str


class BudgetThresholdExceededEvent(Event):
    type: EventType = EventType.BUDGET_THRESHOLD_EXCEEDED
    transactions: list[NoCodeTransaction]
    budget_entries: list[NoCodeBudgetEntry]

    @property
    def budget_table(self) -> Table:
        budget_entries = sorted(self.budget_entries, key=lambda x: x.progress)

        rows = [generate_budget_row(tx) for tx in budget_entries]

        return generate_table(
            headers=["Category", "Target", "Current", "Progress"],
            rows=rows,
            suffix="",
        )


class WeeklyEvent(Event):
    type: EventType = EventType.WEEKLY


class MonthlyEvent(Event):
    type: EventType = EventType.MONTHLY


class DailyEvent(Event):
    type: EventType = EventType.DAILY
