from typing import Annotated, Callable, List, TypeVar

from fastapi import Depends
import numpy as np
from redis import Redis
from redis.commands.search.field import VectorField
from redis.commands.search.indexDefinition import IndexDefinition, IndexType
from redis.commands.search.query import Query

from services.redis_client import get_redis_client
from services.embed import IEmbedder, get_embedder

T = TypeVar('T')

class DocsRetriever:
    _INDEX_NAME_TEMPLATE = "idx:{key}_vss"
    _KEY_PREFIX_TEMPLATE = "{key}:"

    def __init__(
        self,
        embedder: IEmbedder,
        redis_client: Redis,
    ):
        self.embedder = embedder
        self.redis_client = redis_client

    def does_index_exist(self, index_name: str):
        try:
            if self.redis_client.ft(index_name).info():
                return True
            return False
        except:
            return False

    def _make_knn_query(self, k: int):
        return (
            Query("(*)=>[KNN {k} @vector $query_vector AS vector_score]".format(k=k))
            .sort_by("vector_score")
            .return_fields("vector_score")
            .dialect(2)
        )

    def _embed(
        self,
        docs: List[T],
        linearize_func: Callable[[T], str],
        doc_key_prefix: str,
        index_name: str,
    ):
        texts = [linearize_func(doc) for doc in docs]
        embeddings = self.embedder(texts).astype(np.float32).tolist()
        vector_dim = len(embeddings[0])

        pipeline = self.redis_client.pipeline()
        for i, (doc, embedding) in enumerate(zip(docs, embeddings)):
            redis_key = doc_key_prefix + str(i)
            datum = dict(doc=doc, embedding=embedding)
            pipeline.json().set(redis_key, "$", datum)
        pipeline.execute()

        schema = (
            VectorField(
                "$.embedding",
                "FLAT",
                {"TYPE": "FLOAT32", "DIM": vector_dim, "DISTANCE_METRIC": "COSINE"},
                as_name="vector",
            ),
        )
        definition = IndexDefinition(prefix=[doc_key_prefix], index_type=IndexType.JSON)
        self.redis_client.ft(index_name).create_index(
            fields=schema, definition=definition
        )

    def _retrieve(
        self, encoded_query: np.ndarray[np.float32], knn_query: str, index_name: str
    ):
        ids_and_scores = [
            (doc.id, float(doc.vector_score))
            for doc in self.redis_client.ft(index_name)
            .search(knn_query, {"query_vector": encoded_query.tobytes()})
            .docs
        ]
        docs = self.redis_client.json().mget([id for id, _ in ids_and_scores], ".doc")
        return list(zip(docs, [score for _, score in ids_and_scores]))

    def retrieve(
        self,
        key: str,
        queries: List[str],
        docs_getter: Callable[[], List[T]],
        linearize_func: Callable[[T], str] = str,
        k: int = 3,
    ):
        index_name = self._INDEX_NAME_TEMPLATE.format(key=key)
        if not self.does_index_exist(index_name):
            docs = docs_getter()
            doc_key_prefix = self._KEY_PREFIX_TEMPLATE.format(key=key)
            self._embed(
                docs=docs,
                linearize_func=linearize_func,
                doc_key_prefix=doc_key_prefix,
                index_name=index_name,
            )

        encoded_queries = self.embedder(queries).astype(np.float32)
        knn_query = self._make_knn_query(k)
        return [
            self._retrieve(
                encoded_query=encoded_query, knn_query=knn_query, index_name=index_name
            )
            for encoded_query in encoded_queries
        ]


def get_docs_retriever(
    embedder: Annotated[IEmbedder, Depends(get_embedder)],
    redis_client: Annotated[Redis, Depends(get_redis_client)],
):
    return DocsRetriever(embedder, redis_client)
