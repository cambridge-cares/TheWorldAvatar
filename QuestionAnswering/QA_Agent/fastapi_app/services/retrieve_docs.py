from functools import cached_property
from typing import Callable, Iterable, List, TypeVar

import numpy as np
from redis import Redis
from redis.commands.search.field import VectorField
from redis.commands.search.indexDefinition import IndexDefinition, IndexType
from redis.commands.search.query import Query

from .embed import IEmbedder
from .utils.redis import does_index_exist

T = TypeVar("T")


class DocsRetriever:
    _INDEX_NAME_TEMPLATE = "idx:{key}_vss"
    _KEY_PREFIX_TEMPLATE = "{key}:"

    def __init__(
        self,
        embedder: IEmbedder,
        redis_client: Redis,
        key: str,
        docs: Iterable[T],
        linearize_func: Callable[[T], str] = str,
    ):
        self.embedder = embedder
        self.redis_client = redis_client
        self._docs = docs
        self.linearize_func = linearize_func
        self.doc_key_prefix = self._KEY_PREFIX_TEMPLATE.format(key=key)
        self.index_name = self._INDEX_NAME_TEMPLATE.format(key=key)

    @cached_property
    def docs(self):
        return list(self._docs)

    def _make_knn_query(self, k: int):
        return (
            Query("(*)=>[KNN {k} @vector $query_vector AS vector_score]".format(k=k))
            .sort_by("vector_score")
            .return_fields("vector_score")
            .dialect(2)
        )

    def _embed(self):
        texts = [self.linearize_func(doc) for doc in self.docs]
        embeddings = self.embedder(texts).astype(np.float32).tolist()
        vector_dim = len(embeddings[0])

        pipeline = self.redis_client.pipeline()
        for i, (doc, text, embedding) in enumerate(zip(self.docs, texts, embeddings)):
            redis_key = self.doc_key_prefix + str(i)
            datum = dict(doc=doc, linearized_doc=text, embedding=embedding)
            pipeline.json().set(redis_key, "$", datum)
        pipeline.execute()

        schema = (
            VectorField(
                "$.embedding",
                "FLAT",
                {"TYPE": "FLOAT32", "DIM": vector_dim, "DISTANCE_METRIC": "IP"},
                as_name="vector",
            ),
        )
        definition = IndexDefinition(
            prefix=[self.doc_key_prefix], index_type=IndexType.JSON
        )
        self.redis_client.ft(self.index_name).create_index(
            fields=schema, definition=definition
        )

    def _retrieve(self, encoded_query: np.ndarray[np.float32], knn_query: str):
        ids_and_scores = [
            (doc.id, float(doc.vector_score))
            for doc in self.redis_client.ft(self.index_name)
            .search(knn_query, {"query_vector": encoded_query.tobytes()})
            .docs
        ]
        docs = self.redis_client.json().mget([id for id, _ in ids_and_scores], ".doc")
        return list(zip(docs, [score for _, score in ids_and_scores]))

    def retrieve(
        self,
        queries: List[str],
        k: int = 3,
    ):
        if not does_index_exist(self.redis_client, self.index_name):
            self._embed()

        encoded_queries = self.embedder(queries).astype(np.float32)
        knn_query = self._make_knn_query(k)
        return [
            self._retrieve(encoded_query=encoded_query, knn_query=knn_query)
            for encoded_query in encoded_queries
        ]
