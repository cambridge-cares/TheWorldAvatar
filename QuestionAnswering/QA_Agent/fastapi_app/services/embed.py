from abc import ABC, abstractmethod
from functools import cache
from typing import List

from fastapi import Depends
from openai import OpenAI

from .openai_client import get_openai_client


class IEmbedder(ABC):
    @abstractmethod
    def __call__(self, documents: List[str]) -> List[List[float]]:
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
        return [
            x.embedding
            for i in range(0, len(documents), self.chunk_size)
            for x in self.client.embeddings.create(
                input=documents[i : i + self.chunk_size], model=self.model
            ).data
        ]


@cache
def get_embedder():
    return OpenAIEmbedder(client=get_openai_client())
