from collections import defaultdict
from functools import cache
from typing import Annotated

from fastapi import Depends
from redis import Redis

from services.core.kg import KgClient
from services.core.labels_store import IRIWithLabels, LabelsStore
from services.core.redis import get_redis_client
from ..model import Industry
from ..kg import get_sg_factories_ontop_client

@cache
def get_factory_subclasses(bg_client: KgClient):
    query = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX ontocompany: <http://www.theworldavatar.com/kg/ontocompany#>

SELECT DISTINCT ?IRI WHERE {
?IRI rdfs:subClassOf* ontocompany:Factory .
}"""
    return [
        x["IRI"]["value"] for x in bg_client.query(query)["results"]["bindings"]
    ]


def get_sg_factories_bindings(ontop_client: KgClient, bg_client: KgClient):
    query = """PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX ontocompany: <http://www.theworldavatar.com/kg/ontocompany#>
PREFIX ontochemplant: <http://www.theworldavatar.com/kg/ontochemplant#>

SELECT ?IRI ?label WHERE {{
    VALUES ?Type {{ {types} }}
    ?IRI rdf:type ?Type .
    ?IRI rdfs:label ?label .
}}""".format(
        types=" ".join(
            ["<{iri}>".format(iri=iri) for iri in get_factory_subclasses(bg_client)]
        )
    )

    bindings = [
        {k: v["value"] for k, v in binding.items()}
        for binding in ontop_client.query(query)["results"]["bindings"]
    ]
    iri2labels = defaultdict(list)
    for binding in bindings:
        iri2labels[binding["IRI"]].append(binding["label"])

    for iri, labels in iri2labels.items():
        yield IRIWithLabels(IRI=iri, labels=labels)


def get_sg_factories_labels_store(
    redis_client: Annotated[Redis, Depends(get_redis_client)],
    ontop_client: Annotated[KgClient, Depends(get_sg_factories_ontop_client)],
):
    return LabelsStore(
        redis_client=redis_client,
        key_prefix="sg_factories:factories:",
        index_name="idx:sg_factories:factories",
        bindings=get_sg_factories_bindings(ontop_client=ontop_client),
    )
