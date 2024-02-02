from functools import lru_cache
from importlib import resources
import json
import logging
import os
import time
from typing import Annotated, Callable

from fastapi import APIRouter, Depends
from fastapi.responses import StreamingResponse
from openai import OpenAI
from pydantic import BaseModel
import sentencepiece as spm

from services.chatbot import ChatbotClient


class ChatRequest(BaseModel):
    question: str
    data: str


logger = logging.getLogger(__name__)

router = APIRouter()


@lru_cache
def get_openai_client():
    OPENAI_ENDPOINT = os.getenv("OPENAI_ENDPOINT")
    logger.info("Connecting to chatbot at endpoint: " + OPENAI_ENDPOINT)
    return OpenAI(base_url=OPENAI_ENDPOINT, api_key=os.getenv("OPENAI_API_KEY"))


@lru_cache
def get_tokens_counter():
    with resources.as_file(
        resources.files("resources.common").joinpath("tokenizer.model")
    ) as path:
        sp = spm.SentencePieceProcessor(model_file=str(path))

    def get_tokens_num(text: str):
        return len(sp.encode(text))

    return get_tokens_num


def get_chatbot_client(
    openai_client: Annotated[OpenAI, Depends(get_openai_client)],
    tokens_counter: Annotated[Callable[[str], int], Depends(get_tokens_counter)],
):
    return ChatbotClient(
        openai_client=openai_client,
        model=os.getenv("OPENAI_MODEL"),
        input_limit=int(os.getenv("OPENAI_INPUT_LIMIT")),
        tokens_counter=tokens_counter,
    )


@router.post("")
async def chat(
    req: ChatRequest,
    chatbot_client: Annotated[ChatbotClient, Depends(get_chatbot_client)],
):
    logger.info("Request received to chat endpoint with the following request body")
    logger.info(req)

    def generate():
        start = time.time()
        for chunk in chatbot_client.request_stream(req.question, req.data):
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
