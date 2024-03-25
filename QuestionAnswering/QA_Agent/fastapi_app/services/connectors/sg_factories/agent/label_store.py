from functools import cache
from typing import Annotated, Tuple

from fastapi import Depends
from redis import Redis

from services.utils.bindings import agg_iri_label_pairs
from services.core.kg import KgClient
from services.core.label_store import LabelStore
from services.core.redis import get_redis_client
from ..kg import get_sgFactories_bgClient, get_sgFactories_ontopClient


@cache
def get_factory_subclasses(
    bg_client: Annotated[KgClient, Depends(get_sgFactories_bgClient)]
):
    query = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX ontocompany: <http://www.theworldavatar.com/kg/ontocompany#>

SELECT DISTINCT ?IRI WHERE {
?IRI rdfs:subClassOf* ontocompany:Factory .
}"""
    return tuple(
        [x["IRI"]["value"] for x in bg_client.query(query)["results"]["bindings"]]
    )


def sgFactories_bindings_gen(
    factory_subclasses: Tuple[str, ...], ontop_client: KgClient
):
    query = """PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX ontocompany: <http://www.theworldavatar.com/kg/ontocompany#>
PREFIX ontochemplant: <http://www.theworldavatar.com/kg/ontochemplant#>

SELECT ?IRI ?label WHERE {{
    VALUES ?Type {{ {types} }}
    ?IRI rdf:type ?Type .
    ?IRI rdfs:label ?label .
}}""".format(
        types=" ".join(["<{iri}>".format(iri=iri) for iri in factory_subclasses])
    )

    bindings = [
        {k: v["value"] for k, v in binding.items()}
        for binding in ontop_client.query(query)["results"]["bindings"]
    ]
    pairs = [(binding["IRI"], binding["label"]) for binding in bindings]

    for item in agg_iri_label_pairs(pairs):
        yield item


def get_sgFactories_labelStore(
    redis_client: Annotated[Redis, Depends(get_redis_client)],
    ontop_client: Annotated[KgClient, Depends(get_sgFactories_ontopClient)],
    factory_subclasses: Annotated[Tuple[str, ...], Depends(get_factory_subclasses)],
):
    return LabelStore(
        redis_client=redis_client,
        key_prefix="sg_factories:factories:",
        index_name="idx:sg_factories:factories",
        bindings=sgFactories_bindings_gen(
            factory_subclasses=factory_subclasses, ontop_client=ontop_client
        ),
    )
