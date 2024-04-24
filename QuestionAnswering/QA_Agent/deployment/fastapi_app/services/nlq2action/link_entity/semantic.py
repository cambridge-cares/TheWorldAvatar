from typing import Iterable, List

import numpy as np
from redis import Redis
from redis.commands.search.field import TextField, VectorField
from redis.commands.search.indexDefinition import IndexDefinition, IndexType
from redis.commands.search.query import Query
from pydantic.dataclasses import dataclass

from services.core.embed import IEmbedder
from services.core.redis import does_index_exist
from services.utils.itertools_recipes import batched
from .link import IRI, IEntityLinker


@dataclass
class LexiconEntry:
    iri: str
    label: str
    surface_forms: List[str]


class SemanticEntityLinker(IEntityLinker):
    @classmethod
    def _insert_entries_and_create_index(
        cls,
        redis_client: Redis,
        embedder: IEmbedder,
        key_prefix: str,
        index_name: str,
        entries: Iterable[LexiconEntry],
    ):
        offset = 0
        vector_dim = None
        for chunk in batched(entries, 10):
            chunk = list(chunk)
            embeddings = (
                embedder([", ".join(entry.surface_forms) for entry in chunk])
                .astype(np.float32)
                .tolist()
            )
            if vector_dim is None:
                vector_dim = len(embeddings[0])

            pipeline = redis_client.pipeline()
            for i, (entry, embedding) in enumerate(zip(chunk, embeddings)):
                redis_key = key_prefix + str(offset + i)
                doc = dict(
                    iri=entry.iri,
                    label=entry.label,
                    surface_forms=entry.surface_forms,
                    surface_forms_embedding=embedding,
                )
                pipeline.json().set(redis_key, "$", doc)
            pipeline.execute()

            offset += len(chunk)

        if vector_dim is None:
            raise ValueError(
                f"Index {index_name} is not found and must be created. Therefore, `entries` must not be None."
            )

        schema = (
            TextField("$.label", as_name="label"),
            VectorField(
                "$.surface_forms_embedding",
                "FLAT",
                {"TYPE": "FLOAT32", "DIM": vector_dim, "DISTANCE_METRIC": "IP"},
                as_name="vector",
            ),
        )
        definition = IndexDefinition(prefix=[key_prefix], index_type=IndexType.JSON)
        redis_client.ft(index_name).create_index(fields=schema, definition=definition)

    def __init__(
        self,
        redis_client: Redis,
        embedder: IEmbedder,
        key: str,
        entries: Iterable[LexiconEntry],
    ):
        doc_key_prefix = key + ":"
        index_name = f"idx:{key}_vss"

        if not does_index_exist(redis_client, index_name):
            self._insert_entries_and_create_index(
                redis_client=redis_client,
                embedder=embedder,
                key_prefix=doc_key_prefix,
                index_name=index_name,
                entries=entries,
            )

        self.redis_client = redis_client
        self.embedder = embedder
        self.index_name = index_name

    def link(self, surface_form: str, k: int = 3):
        """Perform vector similarity search over candidate surface forms.
        If input surface form exactly matches any label, return the associated IRI as well.
        """
        inverse_label_query = (
            Query('@label:"{label}"'.format(label=surface_form))
            .return_field("$.iri", as_field="iri")
            .dialect(2)
        )
        res = self.redis_client.ft(self.index_name).search(inverse_label_query)
        iris: List[IRI] = [doc.iri for doc in res.docs]

        k -= len(iris)
        if k >= 0:
            encoded_query = self.embedder([surface_form])[0].astype(np.float32)
            knn_query = (
                Query(
                    "(*)=>[KNN {k} @vector $query_vector AS vector_score]".format(k=k)
                )
                .sort_by("vector_score")
                .return_field("$.iri", as_field="iri")
                .dialect(2)
            )
            res = self.redis_client.ft(self.index_name).search(
                knn_query, {"query_vector": encoded_query.tobytes()}
            )
            iris.extend(doc.iri for doc in res.docs)

        return iris
