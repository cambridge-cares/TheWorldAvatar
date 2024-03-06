from abc import ABC, abstractmethod
from functools import cache
import json
from typing import List, Optional, Tuple

from openai import OpenAI


class IFuncCaller(ABC):
    @abstractmethod
    def predict(self, funcs: List[dict], query: str) -> Tuple[str, dict]:
        pass


class OpenAIFuncCaller(IFuncCaller):
    def __init__(self, url: Optional[str] = None, model: str = "gpt-3.5-turbo-0125"):
        self.client = OpenAI(base_url=url)
        self.model = model

    def predict(self, funcs: List[dict], query: str) -> Tuple[str, dict]:
        response = self.client.chat.completions.create(
            model=self.model,
            messages=[{"role": "user", "content": query}],
            tools=[{"type": "function", "function": func} for func in funcs],
            tool_choice="auto",
        )
        func = response.choices[0].message.tool_calls[0].function
        # TODO: type-check and handle error at json.loads
        return func.name, json.loads(func.arguments)


@cache
def get_func_caller():
    return OpenAIFuncCaller()
