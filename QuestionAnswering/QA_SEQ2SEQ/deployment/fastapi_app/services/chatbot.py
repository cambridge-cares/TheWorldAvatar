import json
import logging
import os
from importlib import resources
from typing import Callable, Dict

from openai import OpenAI, Stream
from openai.types.chat.chat_completion_chunk import ChatCompletionChunk

logger = logging.getLogger(__name__)


def binary_search(low: int, high: int, fn: Callable[[int], int], target: int):
    """Performs binary search over the integer space.
    Args:
        low: lower bound of the search space
        high: upper bound of the search space
        fn: a monotonic function over integer domain and range
    """
    if fn(high) <= target:
        return high
    if fn(low) >= target:
        return low

    while low < high - 1:
        mid = (low + high) // 2
        val = fn(mid)
        if val == target:
            return mid
        elif val < target:
            low = mid
        else:
            high = mid
    return low


class ChatbotClient:
    PROMPT_TEMPLATE: Dict[str, str] = json.loads(
        resources.files("resources.common").joinpath("prompt_template.json").read_text()
    )
    TOKENS_LIMIT = int(os.getenv("OPENAI_TOKENS_LIMIT", 500))

    def __init__(self, openai_client: OpenAI, tokens_counter: Callable[[str], int]):
        self.openai_client = openai_client
        self.count_tokens = tokens_counter

    def request_stream(self, question: str, data: str) -> Stream[ChatCompletionChunk]:
        content = self.PROMPT_TEMPLATE["user"].format(question=question, data=data)

        if self.count_tokens(content) > self.TOKENS_LIMIT:
            logger.info(
                "The supplied data exceeds the token limit of {num} tokens. The data will be truncated.".format(
                    num=self.TOKENS_LIMIT
                )
            )
            content = self.PROMPT_TEMPLATE["user"].format(question=question, data=data)
            truncate_idx = binary_search(
                low=0,
                high=len(content),
                fn=lambda idx: self.count_tokens(content[:idx]),
                target=self.TOKENS_LIMIT,
            )
            content = content[:truncate_idx]

        return self.openai_client.chat.completions.create(
            model=os.getenv("CHATBOT_MODEL"),
            messages=[
                {
                    "role": "system",
                    "content": self.PROMPT_TEMPLATE["system"],
                },
                {
                    "role": "user",
                    "content": content,
                },
            ],
            stream=True,
        )
