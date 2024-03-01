from abc import ABC, abstractmethod
from functools import cache
import json
from typing import List, Tuple

from openai import OpenAI

from services.openai_client import get_openai_client


class IFuncCallPredictor(ABC):
    @abstractmethod
    def predict(self, tools: List[dict], query: str) -> Tuple[str, dict]:
        pass

class OpenAIFuncCallPredictor(IFuncCallPredictor):
    def __init__(self, client: OpenAI, model: str = "gpt-3.5-turbo-0125"):
        self.client = client
        self.model = model

    def predict(self, tools: List[dict], query: str) -> Tuple[str, dict]:
        response = self.client.chat.completions.create(
            model=self.model,
            messages=[{"role": "user", "content": query}],
            tools=tools,
            tool_choice="auto",
        )
        func = response.choices[0].message.tool_calls[0].function
        # TODO: type-check and handle error at json.loads
        return func.name, json.loads(func.arguments)
    
@cache
def get_func_call_predictor():
    return OpenAIFuncCallPredictor(client=get_openai_client())
