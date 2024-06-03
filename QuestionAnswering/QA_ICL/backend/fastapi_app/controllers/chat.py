from functools import cache
import json
import logging
from typing import Annotated, Callable, Optional

from fastapi import Depends
from openai import OpenAI, Stream
from openai.types.chat.chat_completion_chunk import ChatCompletionChunk
import tiktoken

from config import AppSettings, get_app_settings
from services.exceptions import QARequestArtifactNotFound
from services.qa_artifact_store import QARequestArtifactStore, get_qaReq_artifactStore

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


class ChatController:
    PROMPT_TEMPLATE = """### Input question:
{question}

### Semantically parsed query:
{data_req}

### Query execution results:
{data}

Given the context information, please answer the query."""

    def __init__(
        self,
        qa_req_artifact_store: QARequestArtifactStore,
        openai_base_url: Optional[str],
        openai_api_key: Optional[str],
        openai_model: str,
        token_limit: int = 16000,
    ):
        self.qa_req_artifact_store = qa_req_artifact_store
        self.openai_client = OpenAI(base_url=openai_base_url, api_key=openai_api_key)
        self.model = openai_model
        self.tokenizer = tiktoken.get_encoding("cl100k_base")
        self.token_limit = token_limit

    def _count_tokens(self, text: str):
        return len(self.tokenizer.encode(text))

    def request_stream(self, qa_request_id: str) -> Stream[ChatCompletionChunk]:
        artifact = self.qa_req_artifact_store.load(qa_request_id)
        if not artifact:
            raise QARequestArtifactNotFound()

        msg = self.PROMPT_TEMPLATE.format(
            question=artifact.nlq, data_req=artifact.data_req, data=artifact.data
        )

        if self._count_tokens(msg) > self.token_limit:
            if (
                self._count_tokens(
                    self.PROMPT_TEMPLATE.format(
                        question=artifact.nlq, data_req=artifact.data_req, data=""
                    )
                )
                > self.token_limit
            ):
                raise Exception("Token limit is too low!!")

            logger.info(
                "The supplied data exceeds the token limit of {num} tokens. The data will be truncated.".format(
                    num=self.token_limit
                )
            )
            serialized_data = json.dumps(artifact.data)

            truncate_idx = binary_search(
                low=0,
                high=len(msg),
                fn=lambda idx: self._count_tokens(
                    self.PROMPT_TEMPLATE.format(
                        question=artifact.nlq,
                        data_req=artifact.data_req,
                        data=serialized_data[:idx],
                    )
                ),
                target=self.token_limit,
            )
            msg = self.PROMPT_TEMPLATE.format(
                question=artifact.nlq,
                data_req=artifact.data_req,
                data=serialized_data[:truncate_idx],
            )

        return self.openai_client.chat.completions.create(
            model=self.model,
            messages=[
                {
                    "role": "user",
                    "content": msg,
                },
            ],
            stream=True,
        )


@cache
def get_chatbot_client(
    settings: Annotated[AppSettings, Depends(get_app_settings)],
    qa_req_artifact_store: Annotated[
        QARequestArtifactStore, Depends(get_qaReq_artifactStore)
    ],
):
    return ChatController(
        qa_req_artifact_store=qa_req_artifact_store,
        openai_base_url=settings.chat.base_url,
        openai_api_key=settings.chat.api_key,
        openai_model=settings.chat.model,
    )
