import os
import requests
from typing import List, Optional, Type, TypeVar
from pydantic import BaseModel


class ChatMessage(BaseModel):
    role: str
    content: str

type Prompt = list[ChatMessage]


class ChatRequest(BaseModel):
    model: str = "gpt-4o"
    messages: List[ChatMessage]
    response_format: dict
    temperature: float = 0.0
    max_tokens: int = 9000


class ChatChoice(BaseModel):
    message: ChatMessage


class ChatResponse(BaseModel):
    choices: List[ChatChoice]


# Generic Pydantic Model Type
T = TypeVar("T", bound=BaseModel)


def make_chat_request(model: Type[T], messages: List[ChatMessage]) -> Optional[T]:

    api_key = os.getenv("OPENAI_API_KEY")
    if not api_key:
        raise ValueError("Environment variable 'OPENAI_API_KEY' not set.")

    url = "https://api.openai.com/v1/chat/completions"
    headers = {
        "Content-Type": "application/json",
        "Authorization": f"Bearer {api_key}",
    }

    schema = model.model_json_schema()

    chat_request = ChatRequest(
        messages=messages,
        response_format=schema
    )

    try:
        response = requests.post(url, headers=headers, json=chat_request.dict(), timeout=120)

        if response.status_code != 200:
            raise ValueError(f"OpenAI API error: {response.status_code} - {response.text}")

        response_data = response.json()
        return model.model_validate(response_data)

    except requests.RequestException as e:
        print(f"HTTP Error: {e}")
        return None
