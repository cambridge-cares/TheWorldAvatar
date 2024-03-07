from functools import cache
import json

from services.redis_client import get_redis_client
from .kg_client import get_ontospecies_kg_client


class OntoSpeciesLiteralStore:
    CHEMICAL_CLASSES_KEYNAME = "ontospecies:chemical.classes"
    USES_KEYNAME = "ontospecies:uses"
    LABELS_KEYNAME = "ontospecies:labels"

    def __init__(self):
        self.kg_client = get_ontospecies_kg_client()
        self.redis_client = get_redis_client()

    def get_chemical_classes(self):
        if not self.redis_client.exists(self.CHEMICAL_CLASSES_KEYNAME):
            query = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>

SELECT DISTINCT ?Label WHERE {
    ?s a os:ChemicalClass ; rdfs:label ?Label .
}"""
            chemical_classes = [
                x["Label"]["value"]
                for x in self.kg_client.query(query)["results"]["bindings"]
            ]
            self.redis_client.set(
                self.CHEMICAL_CLASSES_KEYNAME, json.dumps(chemical_classes)
            )
            return chemical_classes

        return json.loads(self.redis_client.get(self.CHEMICAL_CLASSES_KEYNAME))

    def get_uses(self):
        if not self.redis_client.exists(self.USES_KEYNAME):
            query = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>

SELECT DISTINCT ?Label WHERE {
    ?s a os:Use ; rdfs:label ?Label .
}"""
            uses = [
                x["Label"]["value"]
                for x in self.kg_client.query(query)["results"]["bindings"]
            ]
            self.redis_client.set(self.USES_KEYNAME, json.dumps(uses))
            return uses

        return json.loads(self.redis_client.get(self.USES_KEYNAME))

    def get_label(self, iri: str):
        if not self.redis_client.hexists(self.LABELS_KEYNAME, iri):
            query = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
            
SELECT DISTINCT ?Label WHERE {{
    <{IRI}> rdfs:label ?Label
}}
LIMIT 1""".format(
                IRI=iri
            )
            bindings = self.kg_client.query(query)["results"]["bindings"]
            if not bindings:
                label = ""
            else:
                label = bindings[0]["Label"]["value"]
            self.redis_client.hset(self.LABELS_KEYNAME, iri, label)
            return label

        return self.redis_client.hget(self.LABELS_KEYNAME, iri)


@cache
def get_ontospecies_literal_store():
    return OntoSpeciesLiteralStore()
