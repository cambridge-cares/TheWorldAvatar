from functools import cache
import json
import logging
from typing import Annotated, Callable

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
        low (int): Lower bound of the search space.
        high (int): Upper bound of the search space.
        fn (Callable[[int], int]): Monotonic function over integer domain and range.
        target (int): Target value to search for.
    
    Returns:
        int: The largest input value for which fn(value) <= target.
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
    """
    Controller for managing chat interactions using OpenAI's API.

    This class handles the construction of prompts, token management,
    and interaction with OpenAI's API to generate responses based on
    QA artefacts.
    """

    INSTRUCTION = """### Instruction: 
- Please answer the input question using the data retrieved from the knowledge base
- Format your response using markdown syntax and do not render tables
- If the provided data do not contain information needed, acknowledge the missing information and respond to the query to the best of your knowledge"""

    @classmethod
    def make_context_input_query(cls, question: str):
        return "### Input question:\n" + question

    @classmethod
    def make_context_rewritten_query(cls, question: str):
        return "### Rewritten question:\n" + question

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
        input_question: str,
        rewritten_question: str | None,
        data,
        data_req=None,
        is_data_truncated: bool = False,
    ):
        """
        Construct the full context message for the OpenAI API.
        
        This method combines various components of the query context
        into a single string to be used as input for the API.
        """
        parts = [
            cls.INSTRUCTION,
            cls.make_context_input_query(input_question),
            (
                cls.make_context_rewritten_query(rewritten_question)
                if rewritten_question and input_question != rewritten_question
                else None
            ),
            cls.make_context_data_req(data_req) if data_req else None,
            cls.make_context_structured_answer(data, is_data_truncated),
        ]
        return "\n\n".join([x for x in parts if x])

    def __init__(
        self,
        qa_req_artifact_store: QARequestArtifactStore,
        openai_base_url: str | None,
        openai_api_key: str | None,
        openai_model: str,
        token_limit: int = 16000,
    ):
        """
        Initialise the ChatController with components and settings.
        """
        self.qa_req_artifact_store = qa_req_artifact_store
        self.openai_client = OpenAI(base_url=openai_base_url, api_key=openai_api_key)
        self.model = openai_model
        self.tokenizer = tiktoken.get_encoding("cl100k_base")
        self.token_limit = token_limit

    def _count_tokens(self, text: str):
        """Count the number of tokens in the given text."""
        return len(self.tokenizer.encode(text))

    def request_stream(self, qa_request_id: str) -> Stream[ChatCompletionChunk]:
        """
        Process a chat request and return a stream of response chunks.
        
        This method retrieves the QA artefact, constructs the API input,
        manages token limits, and streams the API response.
        
        Args:
            qa_request_id (str): ID of the QA request artifact.
        
        Returns:
            Stream[ChatCompletionChunk]: Stream of response chunks from OpenAI API.
        
        Raises:
            QARequestArtifactNotFound: If the specified artifact is not found.
        """

        artifact = self.qa_req_artifact_store.load(qa_request_id)
        if not artifact:
            raise QARequestArtifactNotFound()

        msg = self.make_user_input(
            input_question=artifact.nlq,
            rewritten_question=artifact.nlq_rewritten,
            data_req=artifact.data_req,
            data=artifact.data,
        )

        # Token limit management
        if self._count_tokens(msg) > self.token_limit:
            msg = self.make_user_input(
                input_question=artifact.nlq,
                rewritten_question=artifact.nlq_rewritten,
                data=artifact.data,
            )

        if self._count_tokens(msg) > self.token_limit:
            if (
                self._count_tokens(
                    self.make_user_input(
                        input_question=artifact.nlq,
                        rewritten_question=artifact.nlq_rewritten,
                        data="",
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
                    self.make_user_input(
                        input_question=artifact.nlq,
                        rewritten_question=artifact.nlq_rewritten,
                        data=serialized_data[:idx],
                        is_data_truncated=True,
                    )
                ),
                target=self.token_limit,
            )
            msg = self.make_user_input(
                input_question=artifact.nlq,
                rewritten_question=artifact.nlq_rewritten,
                data=serialized_data[:truncate_idx],
                is_data_truncated=True,
            )

        # Send request to OpenAI API
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
    """
    Factory function to create and cache a ChatController instance.
    
    This function uses FastAPI's dependency injection system to obtain
    necessary dependencies and create a ChatController instance.
    
    Returns:
        ChatController: An instance of the ChatController class.
    """
    return ChatController(
        qa_req_artifact_store=qa_req_artifact_store,
        openai_base_url=settings.chat.base_url,
        openai_api_key=settings.chat.api_key,
        openai_model=settings.chat.model,
    )
