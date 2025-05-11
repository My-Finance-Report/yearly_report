from abc import ABC
from pydantic import BaseModel
from app.schemas.no_code import NoCodeTransaction
from app.models.effect import EventType


def generate_row(transaction: NoCodeTransaction) -> str:
    """Generate an HTML table row for a transaction."""
    # Format the amount with 2 decimal places and add currency symbol
    amount_str = f"${abs(transaction.amount):.2f}"
    # Add negative/positive indicator
    if transaction.kind == "withdrawal":
        amount_class = "negative"
        amount_str = f"-{amount_str}"
    else:
        amount_class = "positive"
        amount_str = f"+{amount_str}"
    
    # Format the date in a readable format
    date_str = transaction.date_of_transaction.strftime("%b %d, %Y")
    
    # Create the HTML row with styling
    return f"""<tr>
        <td>{date_str}</td>
        <td>{transaction.description}</td>
        <td>{transaction.category_name or 'Uncategorized'}</td>
        <td class='{amount_class}'>{amount_str}</td>
    </tr>"""


class Event(ABC, BaseModel):
    type: EventType
    @property
    def alter_settings(self)->str:
        return "Click <a style='text-decoration: underline;' href='https://myfinancereport.com/notifications'>here</a> to change your notification settings"

    @property
    def transactions_table(self) -> str:
        """Generate an HTML table of transactions."""
        if not hasattr(self, "transactions"):
            raise NotImplementedError("must have transactions to build a transaction table")
        
        # Only show up to 5 transactions to keep emails concise
        transactions = getattr(self, 'transactions')
        transactions_to_show = transactions[:5]
        has_more = len(transactions) > 5
        
        # Create rows for each transaction
        rows = "\n".join(generate_row(tx) for tx in transactions_to_show)
        
        # Add a note if there are more transactions than shown
        more_text = f"<tr><td colspan='4'><em>...and {len(transactions) - 5} more transaction(s)</em></td></tr>" if has_more else ""
        
        # Build the complete table with styling
        table = f"""<table style="width:100%; border-collapse: collapse; margin-top: 10px; margin-bottom: 10px;">
    <thead>
        <tr>
            <th style="padding: 8px; text-align: left;">Date</th>
            <th style="padding: 8px; text-align: left;">Description</th>
            <th style="padding: 8px; text-align: left;">Category</th>
            <th style="padding: 8px; text-align: right;">Amount</th>
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
