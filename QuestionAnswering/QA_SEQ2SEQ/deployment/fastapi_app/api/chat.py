from importlib import resources
import json
import logging
import os
import time
from typing import Callable

from fastapi import APIRouter
from fastapi.responses import StreamingResponse
from openai import OpenAI
from pydantic import BaseModel
import sentencepiece as spm


class ChatRequest(BaseModel):
    question: str
    data: str


logger = logging.getLogger(__name__)

router = APIRouter()

CHATBOT_ENDPOINT = os.getenv("CHATBOT_ENDPOINT", "http://localhost:8001/v1")
logger.info("Connecting to chatbot at endpoint: " + CHATBOT_ENDPOINT)
chatbot_client = OpenAI(base_url=CHATBOT_ENDPOINT, api_key="placeholder")

with resources.as_file(
    resources.files("resources.common").joinpath("tokenizer.model")
) as path:
    sp = spm.SentencePieceProcessor(model_file=str(path))

PROMPT_TEMPLATE_FULL_DATA = '''
Query: """
{query}
"""

Data: """
{data}
""""'''
PROMPT_TEMPLATE_TRUNCATED_DATA = '''
Query: """
{query}
"""

Truncated data: """
{data}
""""'''
CHARS_PER_TOKEN = 4
TOKENS_LIMIT = int(os.getenv("CHAT_TOKENS_LIMIT", 500))


def get_tokens_num(text: str):
    return len(sp.encode(text))


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


def make_chatbot_response_stream(question: str, data: str):
    content = PROMPT_TEMPLATE_FULL_DATA.format(query=question, data=data)

    if get_tokens_num(content) > TOKENS_LIMIT:
        logger.info(
            "The supplied data exceeds the token limit of {num} tokens. The data will be truncated.".format(
                num=TOKENS_LIMIT
            )
        )
        content = PROMPT_TEMPLATE_TRUNCATED_DATA.format(query=question, data=data)
        truncate_idx = binary_search(
            low=0,
            high=len(content),
            fn=lambda idx: get_tokens_num(content[:idx]),
            target=TOKENS_LIMIT,
        )
        content = content[:truncate_idx]

    return chatbot_client.chat.completions.create(
        model="placeholder",
        messages=[
            {
                "role": "system",
                "content": "You are a chatbot that succinctly responds to user queries based on the provided data.",
            },
            {
                "role": "user",
                "content": content,
            },
        ],
        stream=True,
    )


@router.post("")
async def chat(req: ChatRequest):
    logger.info("Request received to chat endpoint with the following request body")
    logger.info(req)

    def generate():
        start = time.time()
        for chunk in make_chatbot_response_stream(req.question, req.data):
            content = chunk.choices[0].delta.content
            if content is not None:
                yield "data: {data}\n\n".format(
                    data=json.dumps(
                        {"content": content, "latency": time.time() - start}
                    )
                )

    return StreamingResponse(
        generate(),
        media_type="text/event-stream",
        headers={
            "Content-Type": "text/event-stream",
            "Connection": "keep-alive",
            "Cache-Control": "no-cache",
            "X-Accel-Buffering": "no",  # needed to enable SSE over HTTPS
        },
    )
