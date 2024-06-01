from functools import cache
import logging
import os
from typing import Annotated, Callable, Optional

from fastapi import Depends
from openai import OpenAI, Stream
from openai.types.chat.chat_completion_chunk import ChatCompletionChunk
import tiktoken

from config import AppSettings, get_app_settings

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
    PROMPT_TEMPLATE = """Context information is below.
---------------------
{context_str}
---------------------
Given the context information and not prior knowledge, answer the query.
Query: {query_str}
Answer: """

    def __init__(
        self,
        openai_base_url: Optional[str],
        openai_api_key: Optional[str],
        openai_model: str,
        user_input_tokens_limit: int = 16000,
    ):
        self.openai_client = OpenAI(base_url=openai_base_url, api_key=openai_api_key)
        self.model = openai_model
        self.tokenizer = tiktoken.get_encoding("cl100k_base")
        self.user_input_tokens_limit = user_input_tokens_limit

    def _count_tokens(self, text: str):
        return len(self.tokenizer.encode(text))

    def request_stream(self, question: str, data: str) -> Stream[ChatCompletionChunk]:
        content = self.PROMPT_TEMPLATE.format(context_str=data, query_str=question)

        if self._count_tokens(content) > self.user_input_tokens_limit:
            logger.info(
                "The supplied data exceeds the token limit of {num} tokens. The data will be truncated.".format(
                    num=self.user_input_tokens_limit
                )
            )

            truncate_idx = binary_search(
                low=0,
                high=len(content),
                fn=lambda idx: self._count_tokens(
                    self.PROMPT_TEMPLATE.format(
                        context_str=data[:idx], query_str=question
                    )
                ),
                target=self.user_input_tokens_limit,
            )
            content = self.PROMPT_TEMPLATE.format(
                context_str=data[:truncate_idx], query_str=question
            )

        return self.openai_client.chat.completions.create(
            model=self.model,
            messages=[
                {
                    "role": "user",
                    "content": content,
                },
            ],
            stream=True,
        )


@cache
def get_chatbot_client(settings: Annotated[AppSettings, Depends(get_app_settings)]):
    return ChatbotClient(
        openai_base_url=settings.chat.base_url,
        openai_api_key=settings.chat.api_key,
        openai_model=settings.chat.model,
    )
