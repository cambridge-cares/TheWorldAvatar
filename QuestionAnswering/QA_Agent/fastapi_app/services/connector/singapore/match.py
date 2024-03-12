from typing import Annotated
from fastapi import Depends
import numpy as np
from redis import Redis
from redis.commands.search.field import (
    TextField,
    VectorField,
)
from redis.commands.search.indexDefinition import IndexDefinition, IndexType
from redis.commands.search.query import Query

from services.kg_client import KgClient
from services.redis_client import get_redis_client
from services.embed import IEmbedder, get_embedder
from .constants import LAND_USE_TYPES
from .kg_client import get_singapore_kg_client


class LandUseTypeMatcher:
    _REDIS_KEY_PREFIX = "singapore:land_use_types:"
    _INDEX_NAME = "idx:singapore:land_use_types_vss"
    _KNN_QUERY = (
        Query("(*)=>[KNN 1 @vector $query_vector AS vector_score]")
        .sort_by("vector_score")
        .return_fields("vector_score", "IRI", "label", "comment")
        .dialect(2)
    )

    def __init__(self, kg_client: KgClient, embedder: IEmbedder, redis_client: Redis):
        self.kg_client = kg_client
        self.embedder = embedder
        self.redis_client = redis_client

    def _insert_data(self):
        query = """
SELECT ?IRI ?label ?comment WHERE {{
    VALUES ?IRI {{ {values} }}
    ?IRI rdfs:label ?label .
    ?IRI rdfs:comment ?comment .
}}""".format(
            values=" ".join(["<{iri}>".format(iri=iri) for iri in LAND_USE_TYPES])
        )
        land_use_types = [
            {k: v["value"] for k, v in binding.items()}
            for binding in self.kg_client.query(query)["results"]["bindings"]
        ]

        pipeline = self.redis_client.pipeline()
        for i, obj in enumerate(land_use_types):
            redis_key = self._REDIS_KEY_PREFIX + str(i)
            pipeline.json().set(redis_key, "$", obj)
        pipeline.execute()

    def _embed(self):
        self._insert_data()
        keys = sorted(self.redis_client.keys(self._REDIS_KEY_PREFIX + "*"))

        comments = self.redis_client.json().mget(keys, "$.comment")
        comments = [item for sublist in comments for item in sublist]
        labels = self.redis_client.json().mget(keys, "$.label")
        labels = [item for sublist in labels for item in sublist]

        texts = [
            "label: {label}; comment: {comment}.".format(label=label, comment=comment)
            for label, comment in zip(labels, comments)
        ]
        embeddings = self.embedder(texts).astype(np.float32).tolist()
        vector_dimension = len(embeddings[0])

        pipeline = self.redis_client.pipeline()
        for key, embedding in zip(keys, embeddings):
            pipeline.json().set(key, "$.embeddings", embedding)
        pipeline.execute()

        schema = (
            TextField("$.IRI", no_stem=True, as_name="IRI"),
            TextField("$.label", no_stem=True, as_name="label"),
            TextField("$.comment", as_name="comment"),
            VectorField(
                "$.embeddings",
                "FLAT",
                {
                    "TYPE": "FLOAT32",
                    "DIM": vector_dimension,
                    "DISTANCE_METRIC": "COSINE",
                },
                as_name="vector",
            ),
        )
        definition = IndexDefinition(
            prefix=[self._REDIS_KEY_PREFIX], index_type=IndexType.JSON
        )
        self.redis_client.ft(self._INDEX_NAME).create_index(
            fields=schema, definition=definition
        )

    def _does_index_exist(self):
        try:
            if self.redis_client.ft(self._INDEX_NAME).info():
                return True
            return False
        except:
            return False

    def match(self, query):
        if not self._does_index_exist():
            self._embed()

        encoded_query = self.embedder([query]).astype(np.float32)[0]
        doc = (
            self.redis_client.ft(self._INDEX_NAME)
            .search(self._KNN_QUERY, {"query_vector": encoded_query.tobytes()})
            .docs[0]
        )
        return (doc.IRI, doc.label, doc.comment, doc.vector_score)


def get_land_use_type_matcher(
    kg_client: Annotated[KgClient, Depends(get_singapore_kg_client)],
    embedder: Annotated[IEmbedder, Depends(get_embedder)],
    redis_client: Annotated[Redis, Depends(get_redis_client)],
):
    return LandUseTypeMatcher(
        kg_client=kg_client, embedder=embedder, redis_client=redis_client
    )
