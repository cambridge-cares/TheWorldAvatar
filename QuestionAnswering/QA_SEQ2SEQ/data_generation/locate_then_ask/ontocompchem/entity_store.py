from typing import Dict, Type, Union
from locate_then_ask.kg_client import KgClient
from locate_then_ask.ontocompchem.model import OCCMolecularComputation, OCCSpecies


class OCCEntityStore:
    def __init__(self, kg_endpoint: str, **kwargs):
        self.kg_client = KgClient(kg_endpoint, **kwargs)
        self.iri2cls: Dict[
            str, Union[Type[OCCMolecularComputation], Type[OCCSpecies]]
        ] = dict()
        self.iri2entity: Dict[str, Union[OCCMolecularComputation, OCCSpecies]] = dict()

    def get_cls(self, entity_iri: str):
        if entity_iri not in self.iri2cls:
            query_template = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX purl: <http://purl.org/gc/>
ASK WHERE {{ <{IRI}> a/rdfs:subClassOf* purl:MolecularComputation }}"""
            query = query_template.format(IRI=entity_iri)
            if self.kg_client.query(query)["boolean"]:
                self.iri2cls[entity_iri] = OCCMolecularComputation
            else:
                self.iri2cls[entity_iri] = OCCSpecies
        return self.iri2cls[entity_iri]

    def get(self, entity_iri: str):
        if entity_iri not in self.iri2entity:
            entity_cls = self.get_cls(entity_iri)
            if entity_cls is OCCMolecularComputation:
                self.iri2entity[entity_iri] = self.create_molcomp(entity_iri)
            else:
                self.iri2entity[entity_iri] = self.create_species(entity_iri)
        return self.iri2entity[entity_iri]

    def create_molcomp(self, entity_iri: str):
        species = self.retrieve_molcomp_species(entity_iri)
        level_of_theory = self.retrieve_molcomp_leveloftheory(entity_iri)
        basis_set = self.retrieve_molcomp_basisset(entity_iri)
        return OCCMolecularComputation(
            iri=entity_iri,
            species_iri=species,
            level_of_theory=level_of_theory,
            basis_set=basis_set,
        )

    def retrieve_molcomp_species(self, entity_iri: str):
        query_template = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX occ: <http://www.theworldavatar.com/ontology/ontocompchem/OntoCompChem.owl#>

SELECT DISTINCT ?Species WHERE {{
    <{IRI}> occ:hasSpeciesModel/occ:hasSpecies ?Species .
}}
LIMIT 1"""
        query = query_template.format(IRI=entity_iri)
        response_bindings = self.kg_client.query(query)["results"]["bindings"]
        return response_bindings[0]["Species"]["value"]

    def retrieve_molcomp_leveloftheory(self, entity_iri: str):
        query_template = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX occ: <http://www.theworldavatar.com/ontology/ontocompchem/OntoCompChem.owl#>

SELECT DISTINCT ?LevelOfTheoryLabel WHERE {{
    <{IRI}> occ:hasMethodology/occ:hasLevelOfTheory/rdfs:label ?LevelOfTheoryLabel .
}}"""
        query = query_template.format(IRI=entity_iri)
        response_bindings = self.kg_client.query(query)["results"]["bindings"]
        return response_bindings[0]["LevelOfTheoryLabel"]["value"]

    def retrieve_molcomp_basisset(self, entity_iri: str):
        query_template = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX occ: <http://www.theworldavatar.com/ontology/ontocompchem/OntoCompChem.owl#>

SELECT DISTINCT ?BasisSetLabel WHERE {{
    <{IRI}> occ:hasMethodology/occ:hasBasisSet/rdfs:label ?BasisSetLabel .
}}"""
        query = query_template.format(IRI=entity_iri)
        response_bindings = self.kg_client.query(query)["results"]["bindings"]
        return response_bindings[0]["BasisSetLabel"]["value"]

    def create_species(self, entity_iri: str):
        label = self.retrieve_species_label(entity_iri)
        molcomp_iris = self.retrieve_species_molcomp(entity_iri)

        return OCCSpecies(
            iri=entity_iri, label=label, molecular_computation_iris=molcomp_iris
        )

    def retrieve_species_label(self, entity_iri: str):
        query_template = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
SELECT DISTINCT ?Label WHERE {{
    <{IRI}> rdfs:label ?Label .
}}
LIMIT 1"""
        query = query_template.format(IRI=entity_iri)
        response_bindings = self.kg_client.query(query)["results"]["bindings"]
        return response_bindings[0]["Label"]["value"]

    def retrieve_species_molcomp(self, entity_iri: str):
        query_template = """PREFIX occ: <http://www.theworldavatar.com/ontology/ontocompchem/OntoCompChem.owl#>

SELECT DISTINCT ?MolecularComputation WHERE {{
    ?MolecularComputation occ:hasSpeciesModel/occ:hasSpecies <{IRI}>
}}"""
        query = query_template.format(IRI=entity_iri)
        response_bindings = self.kg_client.query(query)["results"]["bindings"]
        return [x["MolecularComputation"]["value"] for x in response_bindings]
