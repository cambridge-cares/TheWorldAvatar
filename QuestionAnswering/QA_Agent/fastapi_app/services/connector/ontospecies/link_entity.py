from typing import List

from .store import get_ontospecies_literal_store
from .kg_client import get_ontospecies_kg_client


class SpeciesLinker:
    def __init__(self):
        self.kg_client = get_ontospecies_kg_client()
        self.literal_store = get_ontospecies_literal_store()

    def link(self, species: str) -> List[str]:
        # TODO: look-up an in-memory cache of label to IRI mappings
        # TODO: use fuzzy matching
        if species in self.literal_store.get_chemical_classes():
            query = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>

SELECT DISTINCT ?Species WHERE {{
    ?Species (a|!a)+ [ a os:ChemicalClass ; rdfs:label "{chemical_class}" ]
}}""".format(
                chemical_class=species
            )
        else:
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
    }}
}}""".format(
                label=species
            )
        return [
            x["Species"]["value"]
            for x in self.kg_client.query(query)["results"]["bindings"]
        ]
