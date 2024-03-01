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
    def __init__(self, client: OpenAI, model: str = "text-embedding-3-small"):
        self.client = client
        self.model = model

    def __call__(self, documents: List[str]):
        # TODO: handle when `documents` or `queries` is an empty Lists
        return [
            x.embedding
            for x in self.client.embeddings.create(
                input=documents, model=self.model
            ).data
        ]


@cache
def get_embedder():
    return OpenAIEmbedder(client=get_openai_client())
