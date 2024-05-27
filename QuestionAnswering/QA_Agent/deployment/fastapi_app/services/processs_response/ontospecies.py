from functools import cache, lru_cache
from typing import Annotated

from fastapi import Depends
from constants.prefixes import URI_OS
from services.kg import KgClient, get_ontospecies_bgClient
from services.processs_response.expand_response import SparqlResponseExpander
from utils.rdf import flatten_sparql_response


@lru_cache()
def get_species_unique_identifiers(iri: str, kg_client: KgClient):
    query = """PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
    
SELECT * WHERE {{
    VALUES ?Species {{ <{IRI}> }}
    OPTIONAL {{
        ?Species rdfs:label ?Label .
    }}
    OPTIONAL {{
        ?Species os:hasIUPACName/os:value ?IUPACName .
    }}
    OPTIONAL {{
        ?Species os:hasInChI/os:value ?InChI .
    }}
}}
LIMIT 1""".format(
        IRI=iri
    )

    res = kg_client.query(query)
    _, bindings = flatten_sparql_response(res)

    if not bindings:
        return {}

    binding = bindings[0]
    data = {
        "Label": binding["Label"],
        "IUPACName": binding["IUPACName"],
        "InChI": binding["InChI"],
    }

    return {k: v for k, v in data.items() if v}


@cache
def get_ontospecies_responseExpander(
    bg_client: Annotated[KgClient, Depends(get_ontospecies_bgClient)]
):
    return SparqlResponseExpander(
        kg_client=bg_client,
        type2getter={URI_OS + "Species": get_species_unique_identifiers},
    )
