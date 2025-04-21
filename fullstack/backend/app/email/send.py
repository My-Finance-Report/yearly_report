from hashlib import md5
from dataclasses import dataclass, replace
from typing import Callable
from pydantic import BaseModel
import resend
import os

from app.models import User
from app.telegram_utils import send_telegram_message


class EmailArgs(BaseModel):
    pass

@dataclass
class Email():
    subject: str
    html: str
    

def enrich_with_unsubscribe(user: User, email: Email) -> Email:
    domain = os.environ["DOMAIN"]
    unsubscribe_link = f"{domain}/unsubscribe?email={md5(user.email.encode()).hexdigest()}"
    return replace(email, html = f"{email.html}<br/><br/><a href='{unsubscribe_link}'>Unsubscribe from emails</a>")

def send_email(user: User, email_generator: Callable[[], Email])->None:

    resend.api_key = os.environ["RESEND_API_KEY"]

    email = email_generator()
    # TODO enable an unsubscribe flow
    # email = enrich_with_unsubscribe(user, email)
    
    if not user.send_email:
        send_telegram_message(f"User {user.email} has disabled email notifications but triggered an email")
        return

    TESTING=True
    recipient = "mcarroll1220@gmail.com" if TESTING else user.email

    params: resend.Emails.SendParams = {
        "from": "My Financé <matt@updates.myfinancereport.com>",
        "to": [recipient],
        "subject": email.subject,
        "html": email.html
    }

    resend.Emails.send(params)