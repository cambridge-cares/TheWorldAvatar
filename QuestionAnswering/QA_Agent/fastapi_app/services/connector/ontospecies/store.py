from functools import cache

from services.utils.functools import expiring_cache
from .kg_client import get_ontospecies_kg_client


class OntoSpeciesLiteralStore:
    def __init__(self):
        self.kg_client = get_ontospecies_kg_client()


    @expiring_cache()
    def get_chemical_classes(self):
        query = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>

SELECT DISTINCT ?Label WHERE {
    ?s a os:ChemicalClass ; rdfs:label ?Label .
}"""
        return [
            x["Label"]["value"]
            for x in self.kg_client.query(query)["results"]["bindings"]
        ]

    @expiring_cache()
    def get_uses(self):
        query = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>

SELECT DISTINCT ?Label WHERE {
    ?s a os:Use ; rdfs:label ?Label .
}"""
        return [
            x["Label"]["value"]
            for x in self.kg_client.query(query)["results"]["bindings"]
        ]
    
    def get_label(self, iri: str):
        query = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        
SELECT DISTINCT ?Label WHERE {{
    <{IRI}> rdfs:label ?Label
}}""".format(
            IRI=iri
        )
        bindings = self.kg_client.query(query)["results"]["bindings"]
        if not bindings:
            return None
        return bindings[0]["Label"]["value"]

@cache
def get_ontospecies_literal_store():
    return OntoSpeciesLiteralStore()