from abc import ABC, abstractmethod
from typing import List

from openai import OpenAI


class IEmbedder(ABC):
    @abstractmethod
    def __call__(self, documents: List[str]) -> List[List[float]]:
        pass


class OpenAIEmbedder(IEmbedder):
    def __init__(self, client: OpenAI, model: str = "text-embedding-3-small"):
        self.client = client
        self.model = model

    def __call__(self, documents: List[str]):
        return [
            x.embedding
            for x in self.client.embeddings.create(
                input=documents, model=self.model
            ).data
        ]
