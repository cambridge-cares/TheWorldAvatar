from functools import cache
import logging
from typing import Annotated, Any

from fastapi import Depends
import numpy as np
from redis import Redis
from redis.commands.search.query import Query
import regex

from config import (
    AppSettings,
    EntityLinkingConfigEntry,
    EntityLinkingSettings,
    get_app_settings,
)
from model.lexicon import ENTITIES_INDEX_NAME
from services.embed import IEmbedder, get_embedder
from services.redis import get_redis_client
from .base import LinkerManager
from .ontospecies import OntospeciesLinkerManager, get_ontospecies_linkerManager
from .ontozeolite import OntozeoliteLinkerManager, get_ontozeolite_linkerManager
from .ontokin import (
    OntokinLinkerManager,
    get_ontokin_linkerManager,
)
from .ontomops import (
    OntomopsLinkerManager,
    get_ontomops_linkerManager,
)
from .ship import ShipLinkerManager, get_ship_linkerManager

logger = logging.getLogger(__name__)


class CentralEntityLinker:
    def __init__(
        self,
        redis_client: Redis,
        embedder: IEmbedder,
        el_settings: EntityLinkingSettings,
        linker_managers: tuple[LinkerManager, ...],
    ):
        self.redis_client = redis_client
        self.embedder = embedder
        self.vss_threshold = el_settings.semantic.threshold
        self.cls2elconfig = {config.cls: config for config in el_settings.entries}
        self.cls2linker = {
            cls: linker
            for manager in linker_managers
            for cls, linker in manager.cls2linker.items()
        }

    def _match_cls_query(self, cls: str):
        return "@cls:{{{cls}}}".format(
            cls=regex.escape(cls, special_only=False, literal_spaces=True)
        )

    def link_exact(self, surface_form: str, cls: str | None) -> list[str]:
        """Performs exact matching over canonical labels.
        Note: This does not work if either the surface form or stored label contains forward slash.
        """
        match_label = '@label:"{label}"'.format(
            label=regex.escape(self._remove_stop_words(surface_form), special_only=False, literal_spaces=True)
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
            .return_field("vector_score")
            .return_field("$.iri", as_field="iri")
            .dialect(2)
        )
        res = self.redis_client.ft(ENTITIES_INDEX_NAME).search(
            knn_query, {"query_vector": encoded_query.tobytes()}
        )
        docs = res.docs
        doc_iris = [doc.iri for doc in docs]
        doc_scores = [1 - float(doc.vector_score) for doc in docs]
        logger.info("Vector similarity search results: " + str(docs))

        new_iris = [
            iri
            for iri, score in zip(doc_iris, doc_scores)
            if score >= self.vss_threshold
        ]
        if not new_iris:
            idx = max(range(len(docs)), key=lambda i: doc_scores[i])
            new_iris = [doc_iris[idx]]
            logger.info(
                f"All IRIs have similarity scores below {self.vss_threshold} threshold; "
            )
        else:
            logger.info("Filtered IRIs: " + str(new_iris))

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

        return list(set(iris))

    def link(
        self,
        cls: str | None,
        text: str | None,
        identifier: dict[str, Any] = dict(),
        k: int | None = None,
    ):
        logger.info(
            f"Performing entity linking for surface form `{text}` and identifier `{identifier}` of class `{cls}`."
        )

        linker = self.cls2linker.get(cls)
        if linker:
            logger.info(f"Custom entity linker is registered for class `{cls}`.")
            iris = linker(text=text, **identifier)
            if iris:
                logger.info(f"Linked IRIs: {iris}")
                return iris
            else:
                logger.info("Custom entity linker returns no IRIs, fall back to default entity linking logic.")

        config = (
            self.cls2elconfig[cls]
            if cls in self.cls2elconfig
            else EntityLinkingConfigEntry()
        )
        logger.info("Linking strategy: " + str(config))

        k = k if k else config.k
        link_func = (
            self.link_fuzzy if config.strategy == "fuzzy" else self.link_semantic
        )
        texts = [text] if text else []
        texts.extend(x for x in identifier.values() if isinstance(x, str))

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
    
    def _remove_stop_words(self, label: str):
        # Stop words in redis text search can cause errors, so they are removed. Please refer to https://redis.io/docs/latest/develop/interact/search-and-query/advanced-concepts/stopwords/#avoiding-stop-word-detection-in-search-queries for more information
        stop_words_list = set([
            'a', 'is', 'the', 'an', 'and', 'are', 'as', 'at', 'be', 'but', 'by', 
            'for', 'if', 'in', 'into', 'it', 'no', 'not', 'of', 'on', 'or', 
            'such', 'that', 'their', 'then', 'there', 'these', 'they', 'this', 
            'to', 'was', 'will', 'with'
        ])
        
        return " ".join(word for word in label.split() if word not in stop_words_list)


def get_el_configs(app_settings: Annotated[AppSettings, Depends(get_app_settings)]):
    return app_settings.entity_linking


@cache
def get_linker_managers(
    ship_linker_manager: Annotated[ShipLinkerManager, Depends(get_ship_linkerManager)],
    ontospecies_linker_manager: Annotated[
        OntospeciesLinkerManager, Depends(get_ontospecies_linkerManager)
    ],
    ontokin_linker_manager: Annotated[
        OntokinLinkerManager, Depends(get_ontokin_linkerManager)
    ],
    ontozeolite_linker_manager: Annotated[
        OntozeoliteLinkerManager, Depends(get_ontozeolite_linkerManager)
    ],
    ontomops_linker_manager: Annotated[
        OntomopsLinkerManager, Depends(get_ontomops_linkerManager)
    ],
):
    return (
        ontospecies_linker_manager,
        ontokin_linker_manager,
        ontozeolite_linker_manager,
        ontomops_linker_manager,
        ship_linker_manager,
    )


@cache
def get_entity_store(
    redis_client: Annotated[Redis, Depends(get_redis_client)],
    embedder: Annotated[IEmbedder, Depends(get_embedder)],
    el_configs: Annotated[list[EntityLinkingConfigEntry], Depends(get_el_configs)],
    linker_managers: Annotated[tuple[LinkerManager, ...], Depends(get_linker_managers)],
):
    return CentralEntityLinker(
        redis_client=redis_client,
        embedder=embedder,
        el_settings=el_configs,
        linker_managers=linker_managers,
    )
