from abc import ABC
from dataclasses import dataclass
from pydantic import BaseModel
from app.schemas.no_code import NoCodeTransaction
from app.models.effect import EventType


def generate_row(transaction: NoCodeTransaction) -> str:
    """Generate an HTML table row for a transaction."""
    amount_str = f"${abs(transaction.amount):.2f}"
    if transaction.kind == "withdrawal":
        amount_class = "negative"
        amount_str = f"-{amount_str}"
    else:
        amount_class = "positive"
        amount_str = f"+{amount_str}"

    date_str = transaction.date_of_transaction.strftime("%b %d")

    formatted_desc = transaction.description
    if len(formatted_desc) > 10:
        formatted_desc = formatted_desc[:10] + "..."

    return f"""<tr>
        <td>{date_str}</td>
        <td>{formatted_desc}</td>
        <td>{transaction.category_name or "Uncategorized"}</td>
        <td style="text-align: left;" class='{amount_class}'>{amount_str}</td>
    </tr>"""


class Event(ABC, BaseModel):
    type: EventType

    @property
    def alter_settings(self) -> str:
        return "Click <a style='text-decoration: underline;' href='https://myfinancereport.com/notifications'>here</a> to change your notification settings"

    @property
    def transactions_table(self) -> str:
        """Generate an HTML table of transactions."""
        if not hasattr(self, "transactions"):
            raise NotImplementedError(
                "must have transactions to build a transaction table"
            )

        transactions = getattr(self, "transactions")
        transactions_to_show = transactions[:5]
        has_more = len(transactions) > 5

        rows = "\n".join(generate_row(tx) for tx in transactions_to_show)

        more_text = (
            f"<tr><td colspan='4'><em>...and {len(transactions) - 5} more transaction(s)</em></td></tr>"
            if has_more
            else ""
        )

        table = f"""<table style="width:100%; border-collapse: collapse; margin-top: 10px; margin-bottom: 10px;">
    <thead>
        <tr>
            <th style="padding: 8px; text-align: left;">Date</th>
            <th style="padding: 8px; text-align: left;">Desc.</th>
            <th style="padding: 8px; text-align: left;">Category</th>
            <th style="padding: 8px; text-align: left;">Amount</th>
        </tr>
    </thead>
    <tbody>
        {rows}
        {more_text}
    </tbody>
</table>
<style>
    .negative {{ color: #e53e3e; }}
    .positive {{ color: #38a169; }}
</style>"""
        return table


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
