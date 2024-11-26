import json
import logging
from typing import Annotated

from fastapi import APIRouter, Depends
from fastapi.responses import StreamingResponse

from controllers.chat import ChatController, get_chatbot_client
from model.web.chat import ChatRequest


logger = logging.getLogger(__name__)

router = APIRouter()


@router.post("", responses={200: {"content": {"text/event-stream": {}}}})
async def chat(
    req: ChatRequest,
    chatbot_client: Annotated[ChatController, Depends(get_chatbot_client)],
):
    logger.info("Request received to chat endpoint with the following request body")
    logger.info(req)

    def generate():
        for chunk in chatbot_client.request_stream(req.qa_request_id):
            content = chunk.choices[0].delta.content
            if content is not None:
                yield "data: {data}\n\n".format(data=json.dumps({"content": content}))

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
