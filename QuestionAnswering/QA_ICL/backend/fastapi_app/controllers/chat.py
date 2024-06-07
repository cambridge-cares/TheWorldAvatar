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
from services.stores.qa_artifact_store import (
    QARequestArtifactStore,
    get_qaReq_artifactStore,
)

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
    SYSTEM_PROMPT = """You are a helpful assistant that provides concise answers to user queries.

- Always ground your response with the provided data
- Always format your response using markdown syntax and do not render tables
- If only a partial view of the data is provided due to its sheer size, please suggest user to refer to the structured data shown above
- If the provided data do not contain information needed, acknowledge the missing information and respond to the query to the best of your knowledge"""

    @classmethod
    def make_context_input_query(cls, question: str):
        return """### Input question:\n""" + question

    @classmethod
    def make_context_data_req(cls, data_req):
        return "### Semantically parsed query:\n{}".format(data_req)

    @classmethod
    def make_context_structured_answer(cls, data, is_data_truncated: bool = False):
        return "### Query execution results{truncate}:\n{data}".format(
            truncate=" (partial)" if is_data_truncated else "", data=data
        )

    @classmethod
    def make_user_input(
        cls,
        question: str,
        data,
        data_req=None,
        is_data_truncated: bool = False,
    ):
        parts = [
            cls.make_context_input_query(question),
            cls.make_context_data_req(data_req) if data_req else None,
            cls.make_context_structured_answer(data, is_data_truncated),
        ]
        return "\n\n".join([x for x in parts if x])

    PROMPT_TEMPLATE = """### Input question:
{question}

### Semantically parsed query:
{data_req}

### Query execution results:
{data}

Given the context information, please answer the query. For readability, please format your response using markdown syntax but do not render tables."""

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

        msg = self.make_user_input(
            question=artifact.nlq, data_req=artifact.data_req, data=artifact.data
        )

        if self._count_tokens(msg) > self.token_limit:
            # exclude data_req from LLM prompt
            msg = self.make_user_input(question=artifact.nlq, data=artifact.data)

        if self._count_tokens(msg) > self.token_limit:
            if (
                self._count_tokens(self.make_user_input(question=artifact.nlq, data=""))
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
                    self.make_user_input(
                        question=artifact.nlq,
                        data=serialized_data[:idx],
                        is_data_truncated=True,
                    )
                ),
                target=self.token_limit,
            )
            msg = self.make_user_input(
                question=artifact.nlq,
                data=serialized_data[:truncate_idx],
                is_data_truncated=True,
            )

        return self.openai_client.chat.completions.create(
            model=self.model,
            messages=[
                {"role": "system", "content": self.SYSTEM_PROMPT},
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
