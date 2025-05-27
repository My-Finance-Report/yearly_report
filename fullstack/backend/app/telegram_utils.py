import logging

import requests

from app.core.config import settings

logger = logging.getLogger(__name__)


def _get_env() -> str:
    return settings.ENVIRONMENT


def enrich(message: str) -> str:
    return f"{message}\n\n{_get_env()}"


def send_telegram_message(
    message: str, chat_id: str | None = None, disable_notification: bool = False
) -> bool:
    if not settings.telegram_enabled:
        logger.warning("Telegram notifications are disabled. Message not sent.")
        return False

    if _get_env() == "local":
        logger.warning(
            "Telegram notifications are disabled in local environment. Message not sent."
        )
        return False

    try:
        bot_token = settings.TELEGRAM_BOT_TOKEN
        chat_id = chat_id or settings.TELEGRAM_CHAT_ID

        if not bot_token or not chat_id:
            logger.error("Telegram bot token or chat ID is missing")
            return False

        url = f"https://api.telegram.org/bot{bot_token}/sendMessage"
        data = {
            "chat_id": chat_id,
            "text": enrich(message),
            "parse_mode": "HTML",
            "disable_notification": disable_notification,
        }

        requests.post(url, data=data)
        return True
    except Exception as e:
        logger.error(f"Failed to send Telegram message: {str(e)}")
        return False
