class EntityStoreBase:
    from functools import cache


from functools import cache
from importlib.resources import files
import json
import logging
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
from services.embed import IEmbedder, get_embedder

from services.entity_store.base import IEntityLinker
from services.entity_store.ship import ShipLinker, get_ship_linker
from services.entity_store.species import SpeciesLinker, get_species_linker
from services.redis import does_index_exist, get_redis_client
from utils.collections import FrozenDict
from utils.itertools_recipes import batched

EntityIRI = str
EntityLabel = str
ELStrategy = Literal["fuzzy", "semantic"]


@dataclass(frozen=True)
class ELConfig:
    strategy: ELStrategy = "fuzzy"
    k: int = 3


@dataclass(frozen=True)
class ELConfigEntry:
    clsname: str
    el_config: ELConfig


@dataclass(frozen=True)
class LexiconEntry:
    iri: str
    clsname: str
    label: str
    surface_forms: Tuple[str, ...]


logger = logging.getLogger(__name__)


class EntityStore:
    KEY_PREFIX = "entities:"
    INDEX_NAME = "idx:entities"
    _CHUNK_SIZE = 128

    @classmethod
    def _insert_entries_and_create_index(
        cls,
        redis_client: Redis,
        embedder: IEmbedder,
        entries: Iterable[LexiconEntry],
    ):
        offset = 0
        vector_dim = None

        logger.info("Inserting entities into Redis...")
        logger.info("Chunk size: " + str(cls._CHUNK_SIZE))

        for i, chunk in enumerate(batched(entries, cls._CHUNK_SIZE)):
            logger.info("Processing chunk " + str(i))

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

        logger.info("Insertion done")

        if vector_dim is None:
            raise ValueError(
                f"Index {cls.INDEX_NAME} is not found and must be created. Therefore, `entries` must not be None."
            )

        logger.info("Creating index for entities...")

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

        logger.info("Index {index} created".format(index=cls.INDEX_NAME))

    def __init__(
        self,
        redis_client: Redis,
        embedder: IEmbedder,
        lexicon: Iterable[LexiconEntry],
        clsname2elconfig: Dict[str, ELConfig],
        clsname2linker: Dict[str, IEntityLinker],
    ):
        if not does_index_exist(redis_client, self.INDEX_NAME):
            logger.info(
                "Index for entities {index} not found".format(index=self.INDEX_NAME)
            )
            self._insert_entries_and_create_index(
                redis_client=redis_client,
                embedder=embedder,
                entries=lexicon,
            )

        self.redis_client = redis_client
        self.embedder = embedder
        self.clsname2elconfig = clsname2elconfig
        self.clsname2linker = clsname2linker

    def _match_clsname_query(self, clsname: str):
        return "@clsname:{{{clsname}}}".format(
            clsname=regex.escape(clsname, special_only=False, literal_spaces=True)
        )

    def link_exact(self, surface_form: str, clsname: Optional[str]) -> List[str]:
        """Performs exact matching over canonical labels.
        Note: This does not work if either the surface form or stored label contains forward slash.
        """
        match_label = '@label:"{label}"'.format(
            label=regex.escape(surface_form, special_only=False)
        )
        match_clsname = self._match_clsname_query(clsname) if clsname else None

        query_str = (
            "{clsname} {label}".format(clsname=match_clsname, label=match_label)
            if match_clsname
            else match_label
        )
        inverse_label_query = Query(query_str).return_field("$.iri", as_field="iri")

        res = self.redis_client.ft(self.INDEX_NAME).search(inverse_label_query)
        return [doc.iri for doc in res.docs]

    def link_semantic(self, surface_form: str, clsname: Optional[str], k: int):
        """Performs vector similarity search over candidate surface forms.
        If input surface form exactly matches any label, preferentially return the associated IRIs.
        """
        iris = self.link_exact(surface_form=surface_form, clsname=clsname)
        logger.info("IRIs that match surface form exactly: " + str(iris))

        if len(iris) >= k:
            return iris[:k]

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

        new_iris = [doc.iri for doc in res.docs]
        logger.info(
            "IRIs that match surface form based on vector similiarity search: "
            + str(new_iris)
        )

        iris.extend(new_iris)

        return list(set(iris))[:k]

    def link_fuzzy(self, surface_form: str, clsname: Optional[str], k: int):
        """Performs fuzzy search over candidate surface forms.
        If input surface form exactly matches any label, preferentially return the associated IRIs.
        """
        logger.info(
            f"Perform fuzzy linking for `{surface_form}` of class `{clsname}` with k={k}"
        )

        iris = self.link_exact(surface_form=surface_form, clsname=clsname)
        logger.info("IRIs that match surface form exactly: " + str(iris))

        if len(iris) >= k:
            return iris[:k]

        clsname_query = self._match_clsname_query(clsname) if clsname else None
        escaped_words = [
            regex.escape(word, special_only=False, literal_spaces=True)
            for word in surface_form.split()
        ]
        iris_set = set(iris)

        # Currently Redis does not support sorting results by Levenshtein distance.
        # Workaround: Results are ordered by querying with incrementing permissible
        # Levenshtein distance from 0 to 1 (inclusive).
        fz_dist = 0

        while len(iris) < k and fz_dist < 2:
            fuzzy_query = "@surface_forms:{label}".format(
                label=" ".join(
                    "{fz_dist}{word}{fz_dist}".format(word=word, fz_dist="%" * fz_dist)
                    for word in escaped_words
                )
            )
            if clsname_query:
                fuzzy_query = "{clsname_query} {fuzzy_query}".format(
                    clsname_query=clsname_query,
                    fuzzy_query=fuzzy_query,
                )
            query = Query(fuzzy_query).return_field("iri")
            res = self.redis_client.ft(self.INDEX_NAME).search(query)

            for doc in res.docs:
                if doc.iri not in iris_set:
                    iris.append(doc.iri)
                    iris_set.add(doc.iri)
                    logger.info("Fuzzily-matched IRI: " + doc.iri)

            fz_dist += 1

        return iris[:k]

    def link(
        self,
        clsname: str,
        text: Optional[str],
        identifier: Dict[str, str],
        k: Optional[int] = None,
    ):
        logger.info(
            f"Performing entity linking for surface form `{text}` and identifier `{identifier}` of class `{clsname}`."
        )

        if clsname in self.clsname2linker:
            iris = self.clsname2linker[clsname].link(text=text, **identifier)
            if iris:
                return iris

        config = (
            self.clsname2elconfig.get(clsname, ELConfig()) if clsname else ELConfig()
        )
        logger.info("Linking strategy: " + str(config))

        k = k if k else config.k
        if config.strategy == "fuzzy":
            return self.link_fuzzy(text, clsname, k)
        else:
            return self.link_semantic(text, clsname, k)

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
def get_el_configs():
    adapter = TypeAdapter(Tuple[ELConfigEntry, ...])
    return adapter.validate_json(
        files("resources").joinpath("el_config.json").read_text()
    )


@cache
def get_lexicon(configs: Annotated[Tuple[ELConfigEntry, ...], Depends(get_el_configs)]):
    return tuple(
        [
            LexiconEntry(
                iri=obj["iri"],
                clsname=entry.clsname,
                label=obj["label"],
                surface_forms=tuple(obj["surface_forms"]),
            )
            for entry in configs
            for obj in json.loads(
                files("resources.lexicon")
                .joinpath(entry.clsname + "_lexicon.json")
                .read_text()
            )
        ]
    )


@cache
def get_clsname2config(
    config: Annotated[Tuple[ELConfigEntry, ...], Depends(get_el_configs)]
):
    return FrozenDict({entry.clsname: entry.el_config for entry in config})


@cache
def get_clsname2linker(
    ship_linker: Annotated[ShipLinker, Depends(get_ship_linker)],
    species_linker: Annotated[SpeciesLinker, Depends(get_species_linker)],
):
    return FrozenDict({"Ship": ship_linker, "Species": species_linker})


@cache
def get_entity_store(
    redis_client: Annotated[Redis, Depends(get_redis_client)],
    embedder: Annotated[IEmbedder, Depends(get_embedder)],
    lexicon: Annotated[Tuple[LexiconEntry, ...], Depends(get_lexicon)],
    clsname2config: Annotated[FrozenDict[str, ELConfig], Depends(get_clsname2config)],
    clsname2linker: Annotated[
        FrozenDict[str, IEntityLinker], Depends(get_clsname2linker)
    ],
):
    return EntityStore(
        redis_client=redis_client,
        embedder=embedder,
        lexicon=lexicon,
        clsname2elconfig=clsname2config,
        clsname2linker=clsname2linker,
    )
