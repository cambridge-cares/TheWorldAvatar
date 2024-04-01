from dataclasses import asdict
from functools import cache
from typing import Annotated

from fastapi import Depends
from redis import Redis
from pydantic.dataclasses import dataclass

from services.core.embed import IEmbedder, get_embedder
from services.core.redis import get_redis_client
from services.core.kg import KgClient
from services.core.retrieve_docs import DocsRetriever
from .kg import get_sgLandLots_bgClient
from .model import LandUseTypeNode


@cache
def get_landUseTypes(kg_client: KgClient):
    # Currently there are no triples ontozoning:Agriculture rdfs:subClass ontozoning:LandUseType
    # Thus, the detection of LandUseType classes relies on the IRI suffix of LandUseType instances

    query = """SELECT DISTINCT ?IRI ?clsname ?label ?comment WHERE {
BIND (REPLACE(STR(?IRI), "^.*/([^/]*)$", "$1") as ?InstanceName)
FILTER (strstarts(?InstanceName, "LandUseType"))

?IRI rdf:type ?LandUseTypeClass .
BIND (REPLACE(STR(?LandUseTypeClass), "^.*/([^/]*)$", "$1") as ?clsname)

OPTIONAL { ?IRI rdfs:label ?label . }
OPTIONAL { ?IRI rdfs:comment ?comment . }
}"""
    res = kg_client.query(query)
    bindings = [
        {k: v["value"] for k, v in binding.items()}
        for binding in res["results"]["bindings"]
    ]

    return [LandUseTypeNode(**binding) for binding in bindings]


def _linearize_landUseType(datum: LandUseTypeNode):
    return "category: {category}; label: {label}; comment: {comment}.".format(
        category=datum.clsname, label=datum.label, comment=datum.comment
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
        docs=get_landUseTypes(kg_client),
        linearize=_linearize_landUseType,
        jsonify=asdict,
    )
