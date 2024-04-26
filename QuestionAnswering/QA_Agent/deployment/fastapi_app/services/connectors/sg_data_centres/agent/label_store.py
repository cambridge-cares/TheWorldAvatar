from typing import Annotated

from fastapi import Depends
from redis import Redis

from core.kg import KgClient
from core.redis import get_redis_client
from core.label_store import LabelStore
from services.utils.bindings import agg_iri_label_pairs
from services.kg import get_sg_ontopClient


def sgDataCentres_bindings_gen(ontop_client: KgClient):
    query = """PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX ontocompany: <http://www.theworldavatar.com/kg/ontocompany/>

SELECT DISTINCT ?IRI ?company ?label WHERE {
    ?IRI rdf:type ontocompany:DataCentre .
    ?IRI ^ontocompany:isOwnerOf/rdfs:label ?company .
    ?IRI rdfs:label ?label .
}"""
    bindings = [
        {k: v["value"] for k, v in binding.items()}
        for binding in ontop_client.query(query)["results"]["bindings"]
    ]
    pairs = [(binding["IRI"], binding["label"]) for binding in bindings]

    for item in agg_iri_label_pairs(pairs):
        yield item


def get_sgDataCentres_labelStore(
    redis_client: Annotated[Redis, Depends(get_redis_client)],
    ontop_client: Annotated[KgClient, Depends(get_sg_ontopClient)],
):
    return LabelStore(
        redis_client=redis_client,
        key="singapore:data_centres",
        bindings=sgDataCentres_bindings_gen(ontop_client),
    )
