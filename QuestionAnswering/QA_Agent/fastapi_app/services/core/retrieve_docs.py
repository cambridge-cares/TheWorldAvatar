from functools import cached_property
from typing import Callable, Generic, Iterable, List, Optional, TypeVar, Union

import numpy as np
from redis import Redis
from redis.commands.search.field import VectorField, TagField
from redis.commands.search.indexDefinition import IndexDefinition, IndexType
from redis.commands.search.query import Query

from .embed import IEmbedder
from .redis import does_index_exist

T = TypeVar("T")


class DocsRetriever(Generic[T]):
    _INDEX_NAME_TEMPLATE = "idx:{key}_vss"
    _KEY_PREFIX_TEMPLATE = "{key}:"

    def __init__(
        self,
        embedder: IEmbedder,
        redis_client: Redis,
        key: str,
        docs: Iterable[T],
        tags: Iterable[str],
        linearize: Callable[[T], str] = str,
        jsonify: Callable[[T], Union[str, list, dict]] = lambda x: x,
    ):
        self.embedder = embedder
        self.redis_client = redis_client
        self._docs = docs
        self._tags = tags
        self.linearize = linearize
        self.jsonify = jsonify
        self.doc_key_prefix = self._KEY_PREFIX_TEMPLATE.format(key=key)
        self.index_name = self._INDEX_NAME_TEMPLATE.format(key=key)

    @cached_property
    def docs(self):
        return list(self._docs)

    @cached_property
    def tags(self):
        return list(self._tags)

    def _make_knn_query(self, k: int, tag: Optional[str]):
        if tag:
            prefilter = "@tag:{{{tag}}}".format(tag=tag)
        else:
            prefilter = "*"
        return (
            Query(
                "({prefilter})=>[KNN {k} @vector $query_vector AS vector_score]".format(
                    prefilter=prefilter, k=k
                )
            )
            .sort_by("vector_score")
            .return_fields("vector_score")
            .dialect(2)
        )

    def _embed(self):
        texts = [self.linearize(doc) for doc in self.docs]
        embeddings = self.embedder(texts).astype(np.float32).tolist()
        vector_dim = len(embeddings[0])

        pipeline = self.redis_client.pipeline()
        for i, (tag, doc, text, embedding) in enumerate(
            zip(self.tags, self.docs, texts, embeddings)
        ):
            redis_key = self.doc_key_prefix + str(i)
            doc = self.jsonify(doc)
            datum = dict(tag=tag, doc=doc, linearized_doc=text, embedding=embedding)
            pipeline.json().set(redis_key, "$", datum)
        pipeline.execute()

        schema = (
            TagField("$.tag", as_name="tag"),
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

    def retrieve(self, queries: List[str], k: int = 3, tag: Optional[str] = None):
        if not does_index_exist(self.redis_client, self.index_name):
            self._embed()

        encoded_queries = self.embedder(queries).astype(np.float32)
        knn_query = self._make_knn_query(k=k, tag=tag)
        return [
            self._retrieve(encoded_query=encoded_query, knn_query=knn_query)
            for encoded_query in encoded_queries
        ]

    def match(self, query: str, key: Optional[str] = None):
        doc = self.retrieve(queries=[query], k=1)[0][0][0]
        if key:
            return doc[key]
        return doc
