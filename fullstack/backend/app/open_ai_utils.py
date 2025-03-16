import logging
import os
from typing import TypeVar

import openai
from pydantic import BaseModel

logger = logging.getLogger(__name__)


class ChatMessage(BaseModel):
    role: str
    content: str


type Prompt = list[ChatMessage]

T = TypeVar("T", bound=BaseModel)


def make_chat_request(model: type[T], messages: list[ChatMessage]) -> T | None:
    """Send a chat request to OpenAI and return the response as a Pydantic model."""

    api_key = os.getenv("OPENAI_API_KEY")
    if not api_key:
        raise ValueError("Environment variable 'OPENAI_API_KEY' not set.")

    client = openai.OpenAI(api_key=api_key)

    # try:
    response = client.beta.chat.completions.parse(
        model="gpt-4o-mini",
        messages=[msg.model_dump() for msg in messages],  # type: ignore[misc]
        response_format=model,
        temperature=0.0,
        max_tokens=9000,
    )

    if not response.choices:
        raise ValueError("No response choices received from OpenAI.")

    val = response.choices[0].message.parsed
    return val

    # except openai.OpenAIError as e:
    #    print(f"OpenAI API error: {e}")
    #    return None
