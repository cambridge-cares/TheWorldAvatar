import json
import logging
import time
from typing import Annotated, Literal

from fastapi import APIRouter, Depends
from fastapi.responses import StreamingResponse
from pydantic import BaseModel

from services.chat import ChatbotClient, get_chatbot_client


class ChatRequest(BaseModel):
    question: str
    data: str
    mode: Literal["RAG", "2NL"]


logger = logging.getLogger(__name__)

router = APIRouter()


@router.post("")
async def chat(
    req: ChatRequest,
    chatbot_client: Annotated[ChatbotClient, Depends(get_chatbot_client)],
):
    logger.info("Request received to chat endpoint with the following request body")
    logger.info(req)

    def generate():
        start = time.time()
        for chunk in chatbot_client.request_stream(req.question, req.data, req.mode):
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
