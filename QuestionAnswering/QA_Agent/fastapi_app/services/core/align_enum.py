from enum import Enum
from typing import Dict, Generic, Tuple, Type, TypeVar, Union
from redis import Redis

from services.core.embed import IEmbedder
from services.core.retrieve_docs import DocsRetriever

T = TypeVar("T", bound=Enum)
V = TypeVar("V", bound=Enum)


class EnumAligner(Generic[T]):
    def __init__(
        self, embedder: IEmbedder, redis_client: Redis, key: str, enum_cls: Type[T]
    ):
        self.retriever = DocsRetriever(
            embedder=embedder,
            redis_client=redis_client,
            key=key,
            docs=[x.value for x in enum_cls],
        )
        self.enum_cls = enum_cls

    def align(self, value: str):
        aligned, _ = self.align_with_score(value)
        return aligned

    def align_with_score(self, value: str):
        closest, score = self.retriever.retrieve(queries=[value], k=1)[0][0]
        return self.enum_cls(closest), score


class BiEnumAligner(Generic[T, V]):
    def __init__(
        self,
        embedder: IEmbedder,
        redis_client: Redis,
        key: str,
        enum_clses: Tuple[Type[T], Type[V]],
    ):
        tags = [cls.__name__ for cls in enum_clses]
        self.retriever = DocsRetriever(
            embedder=embedder,
            redis_client=redis_client,
            key=key,
            docs=[x.value for enum_cls in enum_clses for x in enum_cls],
            tags=[tag for (tag, enum_cls) in zip(tags, enum_clses) for _ in enum_cls],
        )
        self.enum_clses = enum_clses

    def align(self, value: str, cls: Union[Type[T], Type[V], None] = None):
        closest, _ = self.retriever.retrieve(
            queries=[value], k=1, tag=cls.__name__ if cls else None
        )[0][0]

        if cls:
            return cls(closest)

        for cls in self.enum_clses:
            try:
                return cls(closest)
            except:
                pass

        raise Exception(
            'Retrieved value "{value}" does not match any registered enum classes {classes}.'.format(
                value=closest,
                classes=", ".join(cls.__name__ for cls in self.enum_clses),
            )
        )
