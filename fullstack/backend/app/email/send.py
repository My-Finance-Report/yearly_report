from hashlib import md5
from dataclasses import dataclass, replace
from typing import Callable
from pydantic import BaseModel
import resend
import os

from app.models.user import User
from app.telegram_utils import send_telegram_message


class EmailArgs(BaseModel):
    pass


@dataclass
class Email:
    subject: str
    clean_html: str


def enrich_with_unsubscribe(user: User, email: Email) -> Email:
    domain = os.environ["DOMAIN"]
    unsubscribe_link = (
        f"{domain}/unsubscribe?email={md5(user.email.encode()).hexdigest()}"
    )
    return replace(
        email,
        clean_html=f"{email.clean_html}<br/><br/><a href='{unsubscribe_link}'>Unsubscribe from emails</a>",
    )


def is_test() -> bool:
    return os.environ.get("ENVIRONMENT") == "local"


def send_email(user: User, email_generator: Callable[[], Email]) -> None:
    resend.api_key = os.environ["RESEND_API_KEY"]

    email = email_generator()
    # TODO enable an unsubscribe flow
    # email = enrich_with_unsubscribe(user, email)

    if not user.send_email:
        send_telegram_message(
            f"User {user.email} has disabled email notifications but triggered an email"
        )
        return

    TESTING = is_test()
    TEST_USER_IDS = [1, 4]
    recipient = "mcarroll1220@gmail.com" if TESTING else user.email

    params: resend.Emails.SendParams = {
        "from": "My Financé <matt@updates.myfinancereport.com>",
        "to": [recipient],
        "subject": email.subject,
        "html": email.clean_html,
    }

    # TODO remove once we feel good about the state of the notifications
    if TESTING or user.id not in TEST_USER_IDS:
        print(
            f"Would send email to {recipient} with subject: {email.subject} and html: {email.clean_html}"
        )
    else:
        resend.Emails.send(params)
