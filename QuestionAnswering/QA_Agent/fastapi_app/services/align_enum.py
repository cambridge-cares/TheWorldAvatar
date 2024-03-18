from enum import Enum
from typing import Generic, Type, TypeVar
from redis import Redis

from services.embed import IEmbedder
from services.retrieve_docs import DocsRetriever

E = TypeVar("E", bound=Enum)


class EnumAligner(Generic[E]):
    def __init__(
        self, embedder: IEmbedder, redis_client: Redis, key: str, enum_cls: Type[E]
    ):
        self.retriever = DocsRetriever(
            embedder=embedder,
            redis_client=redis_client,
            key=key,
            docs=[x.value for x in enum_cls],
        )
        self.enum_cls = enum_cls

    def align(self, value: str):
        return self.enum_cls(self.retriever.retrieve(queries=[value], k=1)[0][0][0])
