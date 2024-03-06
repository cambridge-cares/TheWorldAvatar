from abc import ABC, abstractmethod
from functools import cache
from typing import Annotated, List, Literal

import numpy as np
import numpy.typing as npt
from openai import OpenAI
from tritonclient.grpc import InferenceServerClient, InferInput
from transformers import AutoTokenizer

from .openai_client import get_openai_client


class IEmbedder(ABC):
    @abstractmethod
    def __call__(
        self, documents: List[str]
    ) -> Annotated[npt.NDArray[np.float_], Literal["N", "D"]]:
        pass


class OpenAIEmbedder(IEmbedder):
    def __init__(
        self,
        client: OpenAI,
        model: str = "text-embedding-3-small",
        chunk_size: int = 1000,
    ):
        self.client = client
        self.model = model
        self.chunk_size = chunk_size

    def __call__(self, documents: List[str]):
        # TODO: handle when `documents` or `queries` is an empty Lists
        # TODO: pack each chunk to the limit
        return np.array(
            [
                x.embedding
                for i in range(0, len(documents), self.chunk_size)
                for x in self.client.embeddings.create(
                    input=documents[i : i + self.chunk_size], model=self.model
                ).data
            ]
        )


class TritonMPNetEmbedder(IEmbedder):
    def __init__(
        self,
        client: InferenceServerClient,
        triton_model: str = "mpnet",
        tokenizer_model: str = "sentence-transformers/all-mpnet-base-v2",
    ):
        self.client = client
        self.triton_model = triton_model
        self.tokenizer = AutoTokenizer.from_pretrained(tokenizer_model)

    def __call__(self, documents: List[str]):
        encoded = self.tokenizer(documents, padding=True, return_tensors="np")
        input_tensors = [
            InferInput(
                "input_ids", encoded.input_ids.shape, datatype="INT64"
            ).set_data_from_numpy(encoded.input_ids),
            InferInput(
                "attention_mask", encoded.attention_mask.shape, datatype="INT64"
            ).set_data_from_numpy(encoded.attention_mask),
        ]
        response = self.client.infer(model_name=self.triton_model, inputs=input_tensors)
        return response.as_numpy("sentence_embedding")


@cache
def get_embedder():
    return OpenAIEmbedder(client=get_openai_client())
