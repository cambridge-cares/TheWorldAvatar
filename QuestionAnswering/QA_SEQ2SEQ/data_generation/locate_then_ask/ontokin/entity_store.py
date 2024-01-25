from typing import Dict, Type, Union
from constants.namespaces import OKIN, OS
from locate_then_ask.kg_client import KgClient
from locate_then_ask.ontokin.model import (
    OKMechanism,
    OKGasPhaseReaction,
    OKSpecies,
)


class OKEntityStore:
    def __init__(
        self,
        kg_endpoint: str = "http://theworldavatar.com/blazegraph/namespace/ontokin/sparql",
        **kwargs
    ):
        self.kg_client = KgClient(kg_endpoint, **kwargs)
        self.iri2mechanism: Dict[str, OKMechanism] = dict()
        self.iri2rxn: Dict[str, OKGasPhaseReaction] = dict()
        self.iri2species: Dict[str, OKSpecies] = dict()

    def get_mechanism(self, entity_iri: str):
        if entity_iri not in self.iri2mechanism:
            self.iri2mechanism[entity_iri] = self.create_mechanism(entity_iri)
        return self.iri2mechanism[entity_iri]
    
    def get_rxn(self, entity_iri: str):
        if entity_iri not in self.iri2rxn:
            self.iri2rxn[entity_iri] = self.create_rxn(entity_iri)
        return self.iri2rxn[entity_iri]
    
    def get_species(self, entity_iri: str):
        if entity_iri not in self.iri2species:
            self.iri2species[entity_iri] = self.create_species(entity_iri)
        return self.iri2species[entity_iri]

    def create_mechanism(self, entity_iri: str):
        doi = self.retrieve_mechanism_doi(entity_iri)
        species_iris = self.retrieve_mechanism_species_iris(entity_iri)
        reaction_iris = self.retrieve_mechanism_rxn_iris(entity_iri)
        return OKMechanism(
            iri=entity_iri,
            doi=doi,
            species_iris=species_iris,
            reaction_iris=reaction_iris,
        )

    def create_rxn(self, entity_iri: str):
        equations = self.retrieve_rxn_eqns(entity_iri)
        reactants = self.retrieve_rxn_reactants(entity_iri)
        products = self.retrieve_rxn_products(entity_iri)
        mechanisms = self.retrieve_rxn_mechansims(entity_iri)
        return OKGasPhaseReaction(
            iri=entity_iri,
            equations=equations,
            reactant_iris=reactants,
            product_iris=products,
            mechanism_iris=mechanisms,
        )

    def create_species(self, entity_iri: str):
        label = self.retrieve_species_label(entity_iri)
        mechanisms = self.retrieve_species_mechanisms(entity_iri)
        return OKSpecies(
            iri=entity_iri,
            label=label,
            mechanism_iris=mechanisms,
        )

    def retrieve_mechanism_doi(self, entity_iri: str):
        query_template = """PREFIX okin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>
PREFIX op: <http://www.theworldavatar.com/ontology/ontoprovenance/OntoProvenance.owl#>

SELECT DISTINCT * WHERE {{
    <{IRI}> okin:hasProvenance/op:hasDOI ?DOI .
}}
LIMIT 1"""
        query = query_template.format(IRI=entity_iri)
        response_bindings = self.kg_client.query(query)["results"]["bindings"]
        return response_bindings[0]["DOI"]["value"]

    def retrieve_mechanism_species_iris(self, entity_iri: str):
        query_template = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX okin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>
PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>

SELECT DISTINCT ?Species WHERE {{
    <{IRI}> okin:hasGasPhase/^okin:belongsToPhase ?Species .
    ?Species a/rdfs:subClassOf* os:Species .
}}"""
        query = query_template.format(IRI=entity_iri)
        response_bindings = self.kg_client.query(query)["results"]["bindings"]
        return [binding["Species"]["value"] for binding in response_bindings]

    def retrieve_mechanism_rxn_iris(self, entity_iri: str):
        query_template = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX okin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>

SELECT DISTINCT ?Reaction WHERE {{
    <{IRI}> okin:hasReaction ?Reaction .
}}"""
        query = query_template.format(IRI=entity_iri)
        response_bindings = self.kg_client.query(query)["results"]["bindings"]
        return [binding["Reaction"]["value"] for binding in response_bindings]

    def retrieve_rxn_eqns(self, entity_iri: str):
        query_template = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX okin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>

SELECT DISTINCT * WHERE {{
    <{IRI}> okin:hasEquation ?Equation .
}}"""
        query = query_template.format(IRI=entity_iri)
        response_bindings = self.kg_client.query(query)["results"]["bindings"]
        return [binding["Equation"]["value"] for binding in response_bindings]

    def retrieve_rxn_reactants(self, entity_iri: str):
        query_template = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX ocape: <http://www.theworldavatar.com/ontology/ontocape/material/substance/reaction_mechanism.owl#>

SELECT DISTINCT * WHERE {{
    <{IRI}> ocape:hasReactant ?Reactant .
}}"""
        query = query_template.format(IRI=entity_iri)
        response_bindings = self.kg_client.query(query)["results"]["bindings"]
        return [binding["Reactant"]["value"] for binding in response_bindings]

    def retrieve_rxn_products(self, entity_iri: str):
        query_template = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX ocape: <http://www.theworldavatar.com/ontology/ontocape/material/substance/reaction_mechanism.owl#>

SELECT DISTINCT * WHERE {{
    <{IRI}> ocape:hasProduct ?Product .
}}"""
        query = query_template.format(IRI=entity_iri)
        response_bindings = self.kg_client.query(query)["results"]["bindings"]
        return [binding["Product"]["value"] for binding in response_bindings]

    def retrieve_rxn_mechansims(self, entity_iri: str):
        query_template = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX okin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>

SELECT DISTINCT * WHERE {{
    <{ReactionIRI}> ^okin:hasReaction ?Mechanism .
}}"""
        query = query_template.format(ReactionIRI=entity_iri)
        response_bindings = self.kg_client.query(query)["results"]["bindings"]
        return [binding["Mechanism"]["value"] for binding in response_bindings]

    def retrieve_species_label(self, entity_iri: str):
        query_template = """PREFIX skos: <http://www.w3.org/2004/02/skos/core#>

SELECT DISTINCT * WHERE {{
    <{IRI}> skos:altLabel ?Label .
}}
LIMIT 1"""
        query = query_template.format(IRI=entity_iri)
        response_bindings = self.kg_client.query(query)["results"]["bindings"]
        return response_bindings[0]["Label"]["value"]

    def retrieve_species_mechanisms(self, entity_iri: str):
        query_template = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX okin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>

SELECT DISTINCT * WHERE {{
    <{IRI}> okin:belongsToPhase/^okin:hasGasPhase ?Mechanism .
}}"""
        query = query_template.format(IRI=entity_iri)
        response_bindings = self.kg_client.query(query)["results"]["bindings"]
        return [binding["Mechanism"]["value"] for binding in response_bindings]
