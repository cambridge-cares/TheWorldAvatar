from functools import cache
from typing import Annotated, List, Optional

from fastapi import Depends
from services.stores.entity_store.base import IEntityLinker
from services.kg import KgClient, get_ontospecies_bgClient


class SpeciesStore(IEntityLinker):
    IDKEY2PREDKEY = {"inchi": "InChI", "smiles": "SMILES", "iupac_name": "IUPACName"}

    def __init__(self, bg_client: KgClient):
        self.bg_client = bg_client

    def link(self, text: Optional[str], **kwargs) -> List[str]:
        for id_key, pred_key in self.IDKEY2PREDKEY.items():
            if id_key in kwargs:
                query = """PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>

SELECT ?Species WHERE {{
    ?Species a os:Species .
    ?Species os:has{pred_key}/os:value "{id_value}"
}}""".format(
                    pred_key=pred_key, id_value=kwargs[id_key]
                )

                iris = [
                    row["Species"].value
                    for row in self.bg_client.querySelect(query).results.bindings
                ]
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

        return [
            binding["Species"].value
            for binding in self.bg_client.querySelect(query).results.bindings
        ]

    def lookup_iupac_name(self, iri: str) -> Optional[str]:
        query = """PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>

SELECT DISTINCT ?IUPACName
WHERE {{
	<{IRI}> os:hasIUPACName/os:value ?IUPACName .
}}
LIMIT 1""".format(
            IRI=iri
        )
        res = self.bg_client.querySelect(query)
        try:
            return next(binding["IUPACName"].value for binding in res.results.bindings)
        except StopIteration:
            return None


@cache
def get_species_store(
    bg_client: Annotated[KgClient, Depends(get_ontospecies_bgClient)]
):
    return SpeciesStore(bg_client=bg_client)
