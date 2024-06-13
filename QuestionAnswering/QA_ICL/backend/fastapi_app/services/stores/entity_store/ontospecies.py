from functools import cache
from typing import Annotated

from fastapi import Depends
from services.stores.entity_store.base import IEntityLinker
from services.sparql import SparqlClient, get_ontospecies_endpoint


class SpeciesLinker(IEntityLinker):
    IDKEY2PREDKEY = {"inchi": "InChI", "smiles": "SMILES", "iupac_name": "IUPACName"}

    def __init__(self, ontospecies_endpoint: str):
        self.sparql_client = SparqlClient(ontospecies_endpoint)

    def link(self, text: str | None, **kwargs) -> list[str]:
        for id_key, pred_key in self.IDKEY2PREDKEY.items():
            if id_key in kwargs:
                query = """PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>

SELECT ?Species
WHERE {{
    ?Species a os:Species .
    ?Species os:has{pred_key}/os:value "{id_value}"
}}""".format(
                    pred_key=pred_key, id_value=kwargs[id_key]
                )

                _, bindings = self.sparql_client.querySelectThenFlatten(query)
                iris = [row["Species"] for row in bindings]

                if iris:
                    return iris

        texts = list(kwargs.values())
        texts.append(text)
        if not texts:
            return []

        query = """PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>

SELECT ?Species WHERE {{
    ?Species a os:Species .
    VALUES ?Text {{ {texts} }}
    ?Species (((os:hasIUPACName|os:hasMolecularFormula|os:hasSMILES)/os:value)|rdfs:label|skos:altLabel) ?Text .
}}""".format(
            texts=" ".join('"{val}"'.format(val=text) for text in texts)
        )

        _, bindings = self.sparql_client.querySelectThenFlatten(query)
        return [row["Species"] for row in bindings]


@cache
def get_species_linker(endpoint: Annotated[str, Depends(get_ontospecies_endpoint)]):
    return SpeciesLinker(ontospecies_endpoint=endpoint)


class ElementLinker(IEntityLinker):
    def __init__(self, bg_client: SparqlClient):
        self.bg_client = bg_client

    def link(self, text: str | None, **kwargs):
        if "symbol" not in kwargs:
            return []

        query = """PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
        
SELECT DISTINCT *
WHERE {{
    ?Element os:hasElementSymbol/os:value "{symbol}"
}}""".format(
            symbol=kwargs["symbol"]
        )

        _, bindings = self.bg_client.querySelectThenFlatten(query)
        return [binding["Element"] for binding in bindings]


@cache
def get_element_linker(
    bg_client: Annotated[SparqlClient, Depends(get_ontospecies_endpoint)]
):
    return ElementLinker(bg_client=bg_client)
