from functools import cache
from importlib.resources import files
import logging
from typing import Annotated, Dict, List, Optional, Tuple

from fastapi import Depends
import numpy as np
from pydantic import TypeAdapter
from redis import Redis
from redis.commands.search.query import Query
import regex

from services.entity_store.zeolitic_material import (
    ZeoliticMaterialLinker,
    get_zeoliticMaterial_linker,
)
from utils.collections import FrozenDict
from services.embed import IEmbedder, get_embedder
from services.redis import get_redis_client
from .base import IEntityLinker
from .ship import ShipLinker, get_ship_linker
from .species import SpeciesStore, get_species_store
from .zeolite_framework import (
    ZeoliteFrameworkLinker,
    get_zeoliteFramework_linker,
)
from .model import ENTITIES_INDEX_NAME, ELConfig, ELConfigEntry


logger = logging.getLogger(__name__)


class EntityStore:
    def __init__(
        self,
        redis_client: Redis,
        embedder: IEmbedder,
        clsname2elconfig: Dict[str, ELConfig],
        clsname2linker: Dict[str, IEntityLinker],
    ):
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

        res = self.redis_client.ft(ENTITIES_INDEX_NAME).search(inverse_label_query)
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
            res = self.redis_client.ft(ENTITIES_INDEX_NAME).search(query)

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
        link_func = (
            self.link_fuzzy if config.strategy == "fuzzy" else self.link_semantic
        )
        texts = [text] + list(identifier.values())

        iris = []
        for text in texts:
            if len(iris) >= k:
                break
            iris.extend(link_func(text, clsname, k))

        return iris[:k]

    def lookup_label(self, iri: str) -> Optional[str]:
        query = Query(
            "@iri:{{{iri}}}".format(
                iri=regex.escape(iri, special_only=False, literal_spaces=True)
            )
        ).return_field("$.label", as_field="label")
        res = self.redis_client.ft(ENTITIES_INDEX_NAME).search(query)
        return (
            regex.sub(r"\\(.)", r"\1", res.docs[0].label) if len(res.docs) > 0 else None
        )

    def lookup_clsname(self, iri: str) -> Optional[str]:
        query = Query(
            "@iri:{{{iri}}}".format(
                iri=regex.escape(iri, special_only=False, literal_spaces=True)
            )
        ).return_field("$.clsname", as_field="clsname")
        res = self.redis_client.ft(ENTITIES_INDEX_NAME).search(query)
        return res.docs[0].clsname if len(res.docs) > 0 else None


@cache
def get_clsname2config():
    adapter = TypeAdapter(Tuple[ELConfigEntry, ...])
    config = adapter.validate_json(
        files("resources").joinpath("el_config.json").read_text()
    )
    return FrozenDict({entry.clsname: entry.el_config for entry in config})


@cache
def get_clsname2linker(
    ship_linker: Annotated[ShipLinker, Depends(get_ship_linker)],
    species_linker: Annotated[SpeciesStore, Depends(get_species_store)],
    zeolite_framework_linker: Annotated[
        ZeoliteFrameworkLinker, Depends(get_zeoliteFramework_linker)
    ],
    zeolitic_material_linker: Annotated[
        ZeoliticMaterialLinker, Depends(get_zeoliticMaterial_linker)
    ],
):
    return FrozenDict(
        {
            "Ship": ship_linker,
            "Species": species_linker,
            "ZeoliteFramework": zeolite_framework_linker,
            "ZeoliticMaterial": zeolitic_material_linker,
        }
    )


@cache
def get_entity_store(
    redis_client: Annotated[Redis, Depends(get_redis_client)],
    embedder: Annotated[IEmbedder, Depends(get_embedder)],
    clsname2config: Annotated[FrozenDict[str, ELConfig], Depends(get_clsname2config)],
    clsname2linker: Annotated[
        FrozenDict[str, IEntityLinker], Depends(get_clsname2linker)
    ],
):
    return EntityStore(
        redis_client=redis_client,
        embedder=embedder,
        clsname2elconfig=clsname2config,
        clsname2linker=clsname2linker,
    )
