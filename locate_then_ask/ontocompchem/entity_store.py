from typing import Dict
from locate_then_ask.kg_client import KgClient
from locate_then_ask.ontocompchem.model import MolecularComputation


class OCCEntityStore:
    def __init__(
        self,
        kg_endpoint: str = "http://178.128.105.213:3838/blazegraph/namespace/ontocompchem/sparql",
        **kwargs
    ):
        self.kg_client = KgClient(kg_endpoint, **kwargs)
        self.iri2entity: Dict[str, MolecularComputation] = dict()

    def get(self, entity_iri: str):
        if entity_iri not in self.iri2entity:
            self.iri2entity[entity_iri] = self.create(entity_iri)
        return self.iri2entity[entity_iri]

    def create(self, entity_iri: str):
        species = self.retrieve_species(entity_iri)
        level_of_theory = self.retrieve_leveloftheory(entity_iri)
        basis_set = self.retrieve_basisset(entity_iri)
        return MolecularComputation(
            species=species,
            level_of_theory=level_of_theory,
            basis_set=basis_set
        )

    def retrieve_species(self, entity_iri: str):
        query_template = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX occ: <http://www.theworldavatar.com/ontology/ontocompchem/OntoCompChem.owl#>

SELECT DISTINCT ?SpeciesLabel WHERE {{
    <{IRI}> occ:hasSpeciesModel/occ:hasSpecies/rdfs:label ?SpeciesLabel .
}}
LIMIT 1"""
        query = query_template.format(IRI=entity_iri)
        response_bindings = self.kg_client.query(query)["results"]["bindings"]
        return response_bindings[0]["?SpeciesLabel"]["value"]

    def retrieve_leveloftheory(self, entity_iri: str):
        query_template = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX occ: <http://www.theworldavatar.com/ontology/ontocompchem/OntoCompChem.owl#>

SELECT DISTINCT ?LevelOfTheoryLabel WHERE {{
    <{IRI}> occ:hasMethodology/occ:hasLevelOfTheory/rdfs:label ?LevelOfTheoryLabel .
}}"""
        query = query_template.format(IRI=entity_iri)
        response_bindings = self.kg_client.query(query)["results"]["bindings"]
        return response_bindings[0]["?LevelOfTheoryLabel"]["value"]

    def retrieve_basisset(self, entity_iri: str):
        query_template = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX occ: <http://www.theworldavatar.com/ontology/ontocompchem/OntoCompChem.owl#>

SELECT DISTINCT ?BasisSetLabel WHERE {{
    <{IRI}> occ:hasMethodology/occ:hasBasisSet/rdfs:label ?BasisSetLabel .
}}"""
        query = query_template.format(IRI=entity_iri)
        response_bindings = self.kg_client.query(query)["results"]["bindings"]
        return response_bindings[0]["?BasisSetLabel"]["value"]
