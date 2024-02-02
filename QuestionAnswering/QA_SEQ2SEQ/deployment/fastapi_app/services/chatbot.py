import json
import logging
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

    def __init__(
        self,
        openai_client: OpenAI,
        model: str,
        input_limit: int,
        tokens_counter: Callable[[str], int],
    ):
        self.openai_client = openai_client
        self.model = model
        self.input_limit = input_limit - tokens_counter(self.PROMPT_TEMPLATE["system"])
        self.count_tokens = tokens_counter

    def request_stream(self, question: str, data: str) -> Stream[ChatCompletionChunk]:
        content = self.PROMPT_TEMPLATE["user"].format(question=question, data=data)

        if self.count_tokens(content) > self.input_limit:
            logger.info(
                "The supplied data exceeds the token limit of {num} tokens. The data will be truncated.".format(
                    num=self.input_limit
                )
            )
            content = self.PROMPT_TEMPLATE["user"].format(question=question, data=data)
            truncate_idx = binary_search(
                low=0,
                high=len(content),
                fn=lambda idx: self.count_tokens(content[:idx]),
                target=self.input_limit,
            )
            content = content[:truncate_idx]

        return self.openai_client.chat.completions.create(
            model=self.model,
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
