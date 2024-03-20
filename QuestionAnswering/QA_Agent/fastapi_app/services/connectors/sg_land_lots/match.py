from typing import Annotated
from fastapi import Depends
from redis import Redis

from services.core.embed import IEmbedder, get_embedder
from services.core.redis import get_redis_client
from services.core.kg import KgClient
from services.core.retrieve_docs import DocsRetriever
from .constants import LAND_USE_TYPES
from .kg import get_sg_land_lots_bg_client


class LandUseTypeMatcher:
    @classmethod
    def _get_land_use_type_data(cls, kg_client: KgClient):
        query = """
SELECT ?IRI ?label ?comment WHERE {{
    VALUES ?IRI {{ {values} }}
    ?IRI rdfs:label ?label .
    ?IRI rdfs:comment ?comment .
}}""".format(
            values=" ".join(["<{iri}>".format(iri=iri) for iri in LAND_USE_TYPES])
        )
        for binding in kg_client.query(query)["results"]["bindings"]:
            yield {k: v["value"] for k, v in binding.items()}

    @classmethod
    def _linearize_land_use_type_datum(cls, datum: dict):
        return "label: {label}; comment: {comment}.".format(
            label=datum["label"], comment=datum["comment"]
        )

    def __init__(self, embedder: IEmbedder, redis_client: Redis, kg_client: KgClient):
        self.land_use_types_retriever = DocsRetriever(
            embedder=embedder,
            redis_client=redis_client,
            key="sg_land_lots:land_use_types",
            docs=self._get_land_use_type_data(kg_client),
            linearize_func=self._linearize_land_use_type_datum,
        )

    def match(self, query):
        retrieved = self.land_use_types_retriever.retrieve(
            queries=[query],
            k=1,
        )

        return retrieved[0][0][0]["IRI"]


def get_land_use_type_matcher(
    embedder: Annotated[IEmbedder, Depends(get_embedder)],
    redis_client: Annotated[Redis, Depends(get_redis_client)],
    kg_client: Annotated[KgClient, Depends(get_sg_land_lots_bg_client)],
):
    return LandUseTypeMatcher(
        embedder=embedder, redis_client=redis_client, kg_client=kg_client
    )
