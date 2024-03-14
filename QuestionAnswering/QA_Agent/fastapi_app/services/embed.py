from abc import ABC, abstractmethod
from functools import cache
from typing import Annotated, List, Literal, Optional

import numpy as np
import numpy.typing as npt
from openai import OpenAI
from tritonclient.grpc import InferenceServerClient, InferInput
from transformers import AutoTokenizer


from config import TEXT_EMBEDDING_SETTINGS


class IEmbedder(ABC):
    @abstractmethod
    def __call__(
        self, documents: List[str]
    ) -> Annotated[npt.NDArray[np.float_], Literal["N", "D"]]:
        pass


class OpenAIEmbedder(IEmbedder):
    def __init__(
        self,
        url: Optional[str] = None,
        model: str = "text-embedding-3-small",
        chunk_size: int = 1000,
        **kwargs
    ):
        self.client = OpenAI(base_url=url)
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
        url: str = "localhost:8001",
        triton_model: str = "mpnet",
        tokenizer_model: str = "sentence-transformers/all-mpnet-base-v2",
        batch_size: int = 256,
        **kwargs
    ):
        self.client = InferenceServerClient(url)
        self.triton_model = triton_model
        self.tokenizer = AutoTokenizer.from_pretrained(tokenizer_model)
        self.batch_size = batch_size

    def __call__(self, documents: List[str]):
        arrs = []
        for i in range(0, len(documents), self.batch_size):
            batch = documents[i : i + self.batch_size]
            encoded = self.tokenizer(batch, padding=True, return_tensors="np")
            input_tensors = [
                InferInput(
                    "input_ids", encoded.input_ids.shape, datatype="INT64"
                ).set_data_from_numpy(encoded.input_ids),
                InferInput(
                    "attention_mask", encoded.attention_mask.shape, datatype="INT64"
                ).set_data_from_numpy(encoded.attention_mask),
            ]
            response = self.client.infer(
                model_name=self.triton_model, inputs=input_tensors
            )
            arrs.append(response.as_numpy("sentence_embedding"))
        return np.vstack(arrs)


@cache
def get_embedder():
    if TEXT_EMBEDDING_SETTINGS.server == "openai":
        cls = OpenAIEmbedder
    else:
        cls = TritonMPNetEmbedder

    return cls(
        **{
            k: v
            for k, v in TEXT_EMBEDDING_SETTINGS.model_dump().items()
            if v is not None
        }
    )
