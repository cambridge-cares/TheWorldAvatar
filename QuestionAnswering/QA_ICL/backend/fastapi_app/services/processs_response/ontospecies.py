from functools import cache
from typing import Annotated, Sequence

from fastapi import Depends
from services.kg import KgClient, get_ontospecies_bgClient
from services.processs_response.augment_node import NodeDataRetriever
from utils.rdf import flatten_sparql_select_response


def get_species_unique_identifiers(kg_client: KgClient, iris: Sequence[str]):
    query = """PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
    
SELECT *
WHERE {{
    {{
        VALUES ?Species {{ {values} }}
        ?Species rdfs:label ?Label .
    }}
    OPTIONAL {{
        SELECT ?Species (SAMPLE(?IUPACName) AS ?IUPACName)
        WHERE {{
            VALUES ?Species {{ {values} }}
            ?Species os:hasIUPACName/os:value ?IUPACName .
        }}
        GROUP BY ?Species
    }} 
    {{
        VALUES ?Species {{ {values} }}
        ?Species os:hasInChI/os:value ?InChI .
    }}
}}""".format(
        values=" ".join("<{iri}>".format(iri=iri) for iri in iris)
    )

    res = kg_client.querySelect(query)
    _, bindings = flatten_sparql_select_response(res)

    iri2data = {
        binding["Species"]: {k: binding.get(k) for k in ["Label", "IUPACName", "InChI"]}
        for binding in bindings
    }
    iri2data = {
        iri: {k: v for k, v in data.items() if v} for iri, data in iri2data.items()
    }

    return [iri2data.get(iri, {}) for iri in iris]


@cache
def get_ontospecies_nodeDataRetriever(
    bg_client: Annotated[KgClient, Depends(get_ontospecies_bgClient)]
):
    return NodeDataRetriever(
        kg_client=bg_client,
        type2getter={"Species": get_species_unique_identifiers},
    )
