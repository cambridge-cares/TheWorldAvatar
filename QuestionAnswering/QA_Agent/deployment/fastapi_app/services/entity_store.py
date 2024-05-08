from functools import cache
from importlib.resources import files
import json
from typing import Annotated, Dict, Iterable, List, Literal, Optional, Tuple

from fastapi import Depends
import numpy as np
from pydantic import TypeAdapter
from redis import Redis
from redis.commands.search.field import TextField, VectorField, TagField
from redis.commands.search.indexDefinition import IndexDefinition, IndexType
from redis.commands.search.query import Query
from pydantic.dataclasses import dataclass
import regex

from config import QAEngineName, get_qa_engine_name
from core.embed import IEmbedder, get_embedder
from core.redis import does_index_exist, get_redis_client
from utils.collections import FrozenDict
from utils.itertools_recipes import batched

EntityIRI = str
EntityLabel = str
ELStrategy = Literal["fuzzy", "semantic"]


@dataclass(frozen=True)
class ELConfigEntry:
    clsname: str
    el_strategy: ELStrategy


@dataclass(frozen=True)
class LexiconEntry:
    iri: str
    clsname: str
    label: str
    surface_forms: Tuple[str, ...]


class EntityStore:
    KEY_PREFIX = "entities:"
    INDEX_NAME = "idx:entities"

    @classmethod
    def _insert_entries_and_create_index(
        cls,
        redis_client: Redis,
        embedder: IEmbedder,
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
                redis_key = cls.KEY_PREFIX + str(offset + i)
                surface_forms = list(entry.surface_forms)
                if entry.label not in entry.surface_forms:
                    surface_forms.append(entry.label)
                doc = dict(
                    iri=entry.iri,
                    clsname=entry.clsname,
                    label=regex.escape(
                        entry.label, special_only=False, literal_spaces=True
                    ),
                    surface_forms=[
                        regex.escape(sf, special_only=False, literal_spaces=True)
                        for sf in entry.surface_forms
                    ],
                    surface_forms_embedding=embedding,
                )
                pipeline.json().set(redis_key, "$", doc)
            pipeline.execute()

            offset += len(chunk)

        if vector_dim is None:
            raise ValueError(
                f"Index {cls.INDEX_NAME} is not found and must be created. Therefore, `entries` must not be None."
            )

        schema = (
            TagField("$.iri", as_name="iri"),
            TagField("$.clsname", as_name="clsname"),
            TextField("$.label", as_name="label"),
            TextField("$.surface_forms.*", as_name="surface_forms"),
            VectorField(
                "$.surface_forms_embedding",
                "FLAT",
                {"TYPE": "FLOAT32", "DIM": vector_dim, "DISTANCE_METRIC": "IP"},
                as_name="vector",
            ),
        )
        definition = IndexDefinition(prefix=[cls.KEY_PREFIX], index_type=IndexType.JSON)
        redis_client.ft(cls.INDEX_NAME).create_index(
            fields=schema, definition=definition
        )

    def __init__(
        self,
        redis_client: Redis,
        embedder: IEmbedder,
        lexicon: Iterable[LexiconEntry],
        clsname2strategy: Dict[str, ELStrategy] = dict(),
    ):
        if not does_index_exist(redis_client, self.INDEX_NAME):
            self._insert_entries_and_create_index(
                redis_client=redis_client,
                embedder=embedder,
                entries=lexicon,
            )

        self.redis_client = redis_client
        self.embedder = embedder
        self.clsname2strategy = clsname2strategy

    def link_exact(self, surface_form: str) -> List[str]:
        """Performs exact matching over canonical labels.
        Note: This does not work if either the surface form or stored label contains forward slash.
        """
        inverse_label_query = Query(
            '@label:"{label}"'.format(
                label=regex.escape(
                    surface_form, special_only=False, literal_spaces=True
                )
            )
        ).return_field("$.iri", as_field="iri")

        res = self.redis_client.ft(self.INDEX_NAME).search(inverse_label_query)
        return [doc.iri for doc in res.docs]

    def _match_clsname_query(self, clsname: str):
        return "@clsname:{{{clsname}}}".format(
            clsname=regex.escape(clsname, special_only=False, literal_spaces=True)
        )

    def link_semantic(
        self, surface_form: str, clsname: Optional[str] = None, k: int = 3
    ):
        """Performs vector similarity search over candidate surface forms.
        If input surface form exactly matches any label, preferentially return the associated IRIs.
        """
        iris = self.link_exact(surface_form)

        k -= len(iris)
        if k >= 0:
            encoded_query = self.embedder([surface_form])[0].astype(np.float32)
            knn_query = (
                Query(
                    "({filter_query})=>[KNN {k} @vector $query_vector AS vector_score]".format(
                        filter_query=(
                            self._match_clsname_query(clsname) if clsname else "*"
                        ),
                        k=k,
                    )
                )
                .sort_by("vector_score")
                .return_field("$.iri", as_field="iri")
                .dialect(2)
            )
            res = self.redis_client.ft(self.INDEX_NAME).search(
                knn_query, {"query_vector": encoded_query.tobytes()}
            )
            iris.extend(doc.iri for doc in res.docs)

        return list(set(iris))[:k]

    def link_fuzzy(self, surface_form: str, clsname: Optional[str] = None, k: int = 3):
        """Performs fuzzy search over candidate surface forms.
        If input surface form exactly matches any label, preferentially return the associated IRIs.
        """
        iris = self.link_exact(surface_form)
        iris_set = set(iris)

        k -= len(iris)
        if k >= 0:
            clsname_query = self._match_clsname_query(clsname) if clsname else None
            escaped_words = [
                regex.escape(word, special_only=False, literal_spaces=True)
                for word in surface_form.split()
            ]
            # Currently Redis does not support sorting results by Levenshtein distance.
            # Results are ordered by querying with incrementing permissible Levenshtein 
            # distance from 0 to 1 (inclusive).
            for fz_dist in range(2):
                fuzzy_query = "@surface_forms:{label}".format(
                    label=" ".join(
                        "{fz_dist}{word}{fz_dist}".format(
                            word=word, fz_dist="%" * fz_dist
                        )
                        for word in escaped_words
                    )
                )
                if clsname_query:
                    fuzzy_query = "{clsname_query} {fuzzy_query}".format(
                        clsname_query=clsname_query,
                        fuzzy_query=fuzzy_query,
                    )
                query = Query(fuzzy_query).return_field("$.iri", as_field="iri")
                res = self.redis_client.ft(self.INDEX_NAME).search(query)

                for doc in res.docs:
                    if doc.iri not in iris_set:
                        iris.append(doc.iri)
                        iris_set.add(doc.iri)

        return iris[:k]

    def link(self, surface_form: str, clsname: Optional[str] = None, k: int = 3):
        strategy = self.clsname2strategy.get(clsname, "fuzzy") if clsname else "fuzzy"

        if strategy == "fuzzy":
            return self.link_fuzzy(surface_form, clsname, k)
        else:
            return self.link_semantic(surface_form, clsname, k)

    def lookup_label(self, iri: str) -> Optional[str]:
        query = Query(
            "@iri:{{{iri}}}".format(
                iri=regex.escape(iri, special_only=False, literal_spaces=True)
            )
        ).return_field("$.label", as_field="label")
        res = self.redis_client.ft(self.INDEX_NAME).search(query)
        return (
            regex.sub(r"\\(.)", r"\1", res.docs[0].label) if len(res.docs) > 0 else None
        )


@cache
def get_el_config(qa_engine: Annotated[QAEngineName, Depends(get_qa_engine_name)]):
    adapter = TypeAdapter(Tuple[ELConfigEntry, ...])
    return adapter.validate_json(
        files("resources." + qa_engine.value).joinpath("el_config.json").read_text()
    )


@cache
def get_lexicon(config: Annotated[Tuple[ELConfigEntry, ...], Depends(get_el_config)]):
    return tuple(
        [
            LexiconEntry(
                iri=obj["iri"],
                clsname=entry.clsname,
                label=obj["label"],
                surface_forms=tuple(obj["surface_forms"]),
            )
            for entry in config
            for obj in json.loads(
                files("resources.lexicon")
                .joinpath(entry.clsname + "_lexicon.json")
                .read_text()
            )
        ]
    )


@cache
def get_clsname2strategy(
    config: Annotated[Tuple[ELConfigEntry, ...], Depends(get_el_config)]
):
    return FrozenDict({entry.clsname: entry.el_strategy for entry in config})


@cache
def get_entity_store(
    redis_client: Annotated[Redis, Depends(get_redis_client)],
    embedder: Annotated[IEmbedder, Depends(get_embedder)],
    lexicon: Annotated[Tuple[LexiconEntry, ...], Depends(get_lexicon)],
    clsname2strategy: Annotated[
        FrozenDict[str, ELStrategy], Depends(get_clsname2strategy)
    ],
):
    return EntityStore(
        redis_client=redis_client,
        embedder=embedder,
        lexicon=lexicon,
        clsname2strategy=clsname2strategy,
    )
