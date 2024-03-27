from dataclasses import asdict
from typing import Annotated

from fastapi import Depends
from redis import Redis
from pydantic.dataclasses import dataclass

from services.core.embed import IEmbedder, get_embedder
from services.core.redis import get_redis_client
from services.core.kg import KgClient
from services.core.retrieve_docs import DocsRetriever
from .constants import LAND_USE_TYPES
from .kg import get_sgLandLots_bgClient


@dataclass
class LandUseType:
    IRI: str
    label: str
    comment: str

def _landUseTypes_gen(kg_client: KgClient):
    query = """
SELECT ?IRI ?label ?comment WHERE {{
VALUES ?IRI {{ {values} }}
?IRI rdfs:label ?label .
?IRI rdfs:comment ?comment .
}}""".format(
        values=" ".join(["<{iri}>".format(iri=iri) for iri in LAND_USE_TYPES])
    )
    for binding in kg_client.query(query)["results"]["bindings"]:
        yield LandUseType(**{k: v["value"] for k, v in binding.items()})


def _linearize_landUseType(datum: LandUseType):
    return "label: {label}; comment: {comment}.".format(
        label=datum.label, comment=datum.comment
    )


def get_landUseType_retriever(
    embedder: Annotated[IEmbedder, Depends(get_embedder)],
    redis_client: Annotated[Redis, Depends(get_redis_client)],
    kg_client: Annotated[KgClient, Depends(get_sgLandLots_bgClient)],
):
    return DocsRetriever(
        embedder=embedder,
        redis_client=redis_client,
        key="sg_land_lots:land_use_types",
        docs=_landUseTypes_gen(kg_client),
        linearize=_linearize_landUseType,
        jsonify=asdict
    )
