from functools import cache
from typing import Annotated, List
from fastapi import Depends


from services.core.kg import KgClient
from ..kg import get_ontospecies_kg_client


class SpeciesLinker:
    def __init__(self, kg_client: KgClient):
        self.kg_client = kg_client

    @cache
    def link(self, species: str) -> List[str]:
        # TODO: use fuzzy matching
        query = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>

SELECT DISTINCT ?Species WHERE {{
    ?Species a os:Species .
    VALUES ?Label {{ "{label}" }}
    {{
        ?Species rdfs:label ?Label
    }} UNION {{
        ?Species skos:altLabel ?Label
    }} UNION {{
        ?Species ?hasIdentifier [ a/rdfs:subClassOf os:Identifier ; os:value ?Label ]
    }} UNION {{
        ?Species (a|!a)+ [ a os:ChemicalClass ; rdfs:label ?Label ]
    }}
}}""".format(
            label=species
        )
        return [
            x["Species"]["value"]
            for x in self.kg_client.query(query)["results"]["bindings"]
        ]


@cache
def get_species_linker(
    kg_client: Annotated[KgClient, Depends(get_ontospecies_kg_client)]
):
    return SpeciesLinker(kg_client)
