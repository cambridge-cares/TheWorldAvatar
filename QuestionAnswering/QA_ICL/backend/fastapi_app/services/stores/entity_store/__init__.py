from functools import cache
from importlib.resources import files
import itertools
import logging
from typing import Annotated

from fastapi import Depends
import numpy as np
from pydantic import TypeAdapter
from redis import Redis
from redis.commands.search.query import Query
import regex

from config import AppSettings, EntityLinkingConfig, get_app_settings
from services.stores.entity_store.ontokin import (
    MechanismLinker,
    ReactionLinker,
    get_mechanism_linker,
    get_reaction_linker,
)
from utils.collections import FrozenDict
from model.lexicon import ENTITIES_INDEX_NAME, ELConfig, ELConfigEntry
from services.embed import IEmbedder, get_embedder
from services.redis import get_redis_client
from .base import IEntityLinker
from .ship import ShipLinker, get_ship_linker
from .ontospecies import (
    ElementLinker,
    SpeciesLinker,
    get_element_linker,
    get_species_linker,
)
from .ontozeolite import (
    ZeoliteFrameworkLinker,
    ZeoliticMaterialLinker,
    get_zeoliteFramework_linker,
    get_zeoliticMaterial_linker,
)

logger = logging.getLogger(__name__)


class EntityStore:
    def __init__(
        self,
        redis_client: Redis,
        embedder: IEmbedder,
        el_configs: list[EntityLinkingConfig],
        cls2linker: dict[str, IEntityLinker],
    ):
        self.redis_client = redis_client
        self.embedder = embedder
        self.cls2elconfig = {config.cls: config for config in el_configs}
        self.cls2linker = cls2linker

    def _match_cls_query(self, cls: str):
        return "@cls:{{{cls}}}".format(
            cls=regex.escape(cls, special_only=False, literal_spaces=True)
        )

    def link_exact(self, surface_form: str, cls: str | None) -> list[str]:
        """Performs exact matching over canonical labels.
        Note: This does not work if either the surface form or stored label contains forward slash.
        """
        match_label = '@label:"{label}"'.format(
            label=regex.escape(surface_form, special_only=False)
        )
        match_cls = self._match_cls_query(cls) if cls else None

        query_str = (
            "{cls} {label}".format(cls=match_cls, label=match_label)
            if match_cls
            else match_label
        )
        inverse_label_query = Query(query_str).return_field("$.iri", as_field="iri")

        res = self.redis_client.ft(ENTITIES_INDEX_NAME).search(inverse_label_query)
        return [doc.iri for doc in res.docs]

    def link_semantic(self, surface_form: str, cls: str | None, k: int):
        """Performs vector similarity search over candidate surface forms.
        If input surface form exactly matches any label, preferentially return the associated IRIs.
        """
        iris = self.link_exact(surface_form=surface_form, cls=cls)
        logger.info("IRIs that match surface form exactly: " + str(iris))

        if len(iris) >= k:
            return iris

        encoded_query = self.embedder([surface_form])[0].astype(np.float32)
        knn_query = (
            Query(
                "({filter_query})=>[KNN {k} @vector $query_vector AS vector_score]".format(
                    filter_query=(self._match_cls_query(cls) if cls else "*"),
                    k=k,
                )
            )
            .sort_by("vector_score")
            .return_field("$.iri", as_field="iri")
            .dialect(2)
        )
        res = self.redis_client.ft(ENTITIES_INDEX_NAME).search(
            knn_query, {"query_vector": encoded_query.tobytes()}
        )

        new_iris = [doc.iri for doc in res.docs]
        logger.info(
            "IRIs that match surface form based on vector similiarity search: "
            + str(new_iris)
        )

        iris.extend(new_iris)

        return list(set(iris))

    def link_fuzzy(self, surface_form: str, cls: str | None, k: int):
        """Performs fuzzy search over candidate surface forms.
        If input surface form exactly matches any label, preferentially return the associated IRIs.
        """
        logger.info(
            f"Perform fuzzy linking for `{surface_form}` of class `{cls}` with k={k}"
        )

        iris = self.link_exact(surface_form=surface_form, cls=cls)
        logger.info("IRIs that match surface form exactly: " + str(iris))

        if len(iris) >= k:
            return iris

        cls_query = self._match_cls_query(cls) if cls else None
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
            if cls_query:
                fuzzy_query = "{cls_query} {fuzzy_query}".format(
                    cls_query=cls_query,
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

        return iris

    def link(
        self,
        cls: str | None,
        text: str | None,
        identifier: dict[str, str] = dict(),
        k: int | None = None,
    ):
        logger.info(
            f"Performing entity linking for surface form `{text}` and identifier `{identifier}` of class `{cls}`."
        )

        if cls in self.cls2linker:
            iris = self.cls2linker[cls].link(text=text, **identifier)
            if iris:
                return iris

        config = (
            self.cls2elconfig[cls]
            if cls in self.cls2elconfig
            else EntityLinkingConfig()
        )
        logger.info("Linking strategy: " + str(config))

        k = k if k else config.k
        link_func = (
            self.link_fuzzy if config.strategy == "fuzzy" else self.link_semantic
        )
        texts = [x for x in itertools.chain([text], identifier.values()) if x]

        iris: list[str] = []
        for text in texts:
            if len(iris) >= k:
                break
            iris.extend(link_func(surface_form=text, cls=cls, k=k))

        return iris

    def lookup_label(self, iri: str):
        query = Query(
            "@iri:{{{iri}}}".format(
                iri=regex.escape(iri, special_only=False, literal_spaces=True)
            )
        ).return_field("$.label", as_field="label")
        res = self.redis_client.ft(ENTITIES_INDEX_NAME).search(query)
        return (
            regex.sub(r"\\(.)", r"\1", res.docs[0].label) if len(res.docs) > 0 else None
        )

    def lookup_cls(self, iri: str) -> str | None:
        query = Query(
            "@iri:{{{iri}}}".format(
                iri=regex.escape(iri, special_only=False, literal_spaces=True)
            )
        ).return_field("$.cls", as_field="cls")
        res = self.redis_client.ft(ENTITIES_INDEX_NAME).search(query)
        return res.docs[0].cls if len(res.docs) > 0 else None


def get_el_configs(app_settings: Annotated[AppSettings, Depends(get_app_settings)]):
    return app_settings.entity_linking


@cache
def get_cls2linker(
    ship_linker: Annotated[ShipLinker, Depends(get_ship_linker)],
    species_linker: Annotated[SpeciesLinker, Depends(get_species_linker)],
    element_linker: Annotated[ElementLinker, Depends(get_element_linker)],
    mechanism_linker: Annotated[MechanismLinker, Depends(get_mechanism_linker)],
    reaction_linker: Annotated[ReactionLinker, Depends(get_reaction_linker)],
    zeolite_framework_linker: Annotated[
        ZeoliteFrameworkLinker, Depends(get_zeoliteFramework_linker)
    ],
    zeolitic_material_linker: Annotated[
        ZeoliticMaterialLinker, Depends(get_zeoliticMaterial_linker)
    ],
):
    return FrozenDict.from_dict(
        {
            "Ship": ship_linker,
            "os:Species": species_linker,
            "pt:Element": element_linker,
            "okin:ReactionMechanism": mechanism_linker,
            "ocape:ChemicalReaction": reaction_linker,
            "zeo:ZeoliteFramework": zeolite_framework_linker,
            "zeo:ZeoliticMaterial": zeolitic_material_linker,
        }
    )


@cache
def get_entity_store(
    redis_client: Annotated[Redis, Depends(get_redis_client)],
    embedder: Annotated[IEmbedder, Depends(get_embedder)],
    el_configs: Annotated[list[EntityLinkingConfig], Depends(get_el_configs)],
    cls2linker: Annotated[dict[str, IEntityLinker], Depends(get_cls2linker)],
):
    return EntityStore(
        redis_client=redis_client,
        embedder=embedder,
        el_configs=el_configs,
        cls2linker=cls2linker,
    )
