import json
from typing import Iterable, List, Optional, Tuple

import numpy as np
import rapidfuzz
from redis import Redis
from redis.commands.search.field import TextField, VectorField, TagField
from redis.commands.search.indexDefinition import IndexDefinition, IndexType
from redis.commands.search.query import Query
from pydantic.dataclasses import dataclass
import regex

from services.core.embed import IEmbedder
from services.core.redis import does_index_exist
from services.utils.itertools_recipes import batched

EntityIRI = str
EntityLabel = str


@dataclass
class LexiconEntry:
    iri: str
    label: str
    surface_forms: List[str]


class EntityLinker:
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
            TagField("$.iri", as_name="iri"),
            TextField("$.label", as_name="label"),
            TextField("$.surface_forms.*", as_name="surface_forms"),
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
        lexicon: Iterable[LexiconEntry],
    ):
        doc_key_prefix = key + ":"
        index_name = f"idx:{key}_vss"

        if not does_index_exist(redis_client, index_name):
            self._insert_entries_and_create_index(
                redis_client=redis_client,
                embedder=embedder,
                key_prefix=doc_key_prefix,
                index_name=index_name,
                entries=lexicon,
            )

        self.redis_client = redis_client
        self.embedder = embedder
        self.index_name = index_name

    def link_exact(self, surface_form: str) -> List[Tuple[EntityIRI, EntityLabel]]:
        """Performs exact matching over canonical labels."""
        inverse_label_query = (
            Query('@label:"{label}"'.format(label=surface_form))
            .return_field("$.iri", as_field="iri")
            .return_field("$.label", as_field="label")
            .dialect(2)
        )
        res = self.redis_client.ft(self.index_name).search(inverse_label_query)
        return [(doc.iri, doc.label) for doc in res.docs]

    def link_semantic(self, surface_form: str, k: int = 3):
        """Performs vector similarity search over candidate surface forms.
        If input surface form exactly matches any label, preferentially return the associated IRIs.
        """
        iris = self.link_exact(surface_form)

        k -= len(iris)
        if k >= 0:
            encoded_query = self.embedder([surface_form])[0].astype(np.float32)
            knn_query = (
                Query(
                    "(*)=>[KNN {k} @vector $query_vector AS vector_score]".format(k=k)
                )
                .sort_by("vector_score")
                .return_field("$.iri", as_field="iri")
                .return_field("$.label", as_field="label")
                .dialect(2)
            )
            res = self.redis_client.ft(self.index_name).search(
                knn_query, {"query_vector": encoded_query.tobytes()}
            )
            iris.extend((doc.iri, doc.label) for doc in res.docs)

        return iris

    def _all_surface_forms(self) -> List[str]:
        # TODO: accumulate pages from Redis to ensure all labels are retrieved
        query = (
            Query("*")
            .return_field("$.surface_forms", as_field="surface_forms_serialized")
            .paging(0, 10000)
        )
        res = self.redis_client.ft(self.index_name).search(query)
        surface_forms = [
            sf for doc in res.docs for sf in json.loads(doc.surface_forms_serialized)
        ]
        return list(set(surface_forms))

    def _lookup_iris(self, surface_form: str) -> List[str]:
        query = Query(
            "@surface_forms:{{{surface_form}}}".format(
                label=regex.escape(surface_form, special_only=False)
            )
        ).return_field("iri")
        res = self.redis_client.ft(self.index_name).search(query).docs
        iris = [doc.iri for doc in res.docs]
        return list(set(iris))

    def link_fuzzy(self, surface_form: str, k: int = 3):
        """Performs fuzzy search over candidate surface forms.
        If input surface form exactly matches any label, preferentially return the associated IRIs.
        """
        iris = self.link_exact(surface_form)

        k -= len(iris)
        if k >= 0:
            choices = self._all_surface_forms()
            lst = rapidfuzz.process.extract(
                surface_form,
                choices,
                scorer=rapidfuzz.fuzz.WRatio,
                limit=k,
                processor=rapidfuzz.utils.default_process,
            )
            surface_forms = [sf for sf, _, _ in lst]
            iris.extend([iri for sf in surface_forms for iri in self._lookup_iris(sf)])

        return iris

    def lookup_label(self, iri: str) -> Optional[str]:
        query = Query(
            "@iri:{{{iri}}}".format(iri=regex.escape(iri, special_only=False))
        ).return_field("$.label", as_field="label")
        res = self.redis_client.ft(self.index_name).search(query)
        return res.docs[0].label if len(res.docs) > 0 else None
