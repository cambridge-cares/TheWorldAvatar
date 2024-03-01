import logging
import os
from typing import Dict, List, Tuple, Type

from services.utils.functools import expiring_cache
from services.utils.parse import parse_constraint
from services.nearest_neighbor import NNRetriever
from services.kg_client import KgClient
from model.constraint import CompoundNumericalConstraint
from services.connector.agent import IAgent
from .constants import (
    SpeciesAttrKey,
    SpeciesChemicalClassAttrKey,
    SpeciesUseAttrKey,
    SpeciesIdentifierAttrKey,
    SpeciesPropertyAttrKey,
)


logger = logging.getLogger(__name__)


class OntoSpeciesAgent(IAgent):
    _SPECIES_ATTR_CLSES: List[Type[SpeciesAttrKey]] = [
        SpeciesChemicalClassAttrKey,
        SpeciesUseAttrKey,
        SpeciesIdentifierAttrKey,
        SpeciesPropertyAttrKey,
    ]
    _SPECIES_ATTR_KEYS = [x.value for cls in _SPECIES_ATTR_CLSES for x in cls]

    def __init__(self, nn_retriever: NNRetriever):
        self.kg_client = KgClient(os.getenv("KG_ENDPOINT_ONTOSPECIES"))
        self.nn_retriever = nn_retriever

    @expiring_cache()
    def _get_chemical_classes(self):
        query = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>

SELECT DISTINCT ?Label WHERE {
    ?s a os:ChemicalClass ; rdfs:label ?Label .
}"""
        return [
            x["Label"]["value"]
            for x in self.kg_client.query(query)["results"]["bindings"]
        ]

    @classmethod
    def get_tools(cls):
        return [
            {
                "type": "function",
                "function": {
                    "name": "lookup_chemicalSpecies_attributes",
                    "description": "Given a chemical species or chemical class, retrieve its requested attributes e.g. chemical class, application, boiling point, molecular formula",
                    "parameters": {
                        "type": "object",
                        "properties": {
                            "species": {
                                "type": "string",
                                "description": "A common name, molecular formula, or chemical class e.g. benzene, H2O, alcohol",
                            },
                            "attributes": {
                                "type": "array",
                                "items": {
                                    "type": "string",
                                    "description": "Attribute to retrieve e.g. charge, usage, InChI",
                                },
                            },
                        },
                    },
                },
            },
            {
                "type": "function",
                "function": {
                    "name": "find_chemicalSpecies",
                    "description": "Find chemical species given some criteria",
                    "parameters": {
                        "type": "object",
                        "properties": {
                            "chemical_classes": {
                                "type": "array",
                                "items": {
                                    "type": "string",
                                    "description": "Name of the desired chemical class e.g. aldehyde",
                                },
                            },
                            "uses": {
                                "type": "array",
                                "items": {
                                    "type": "string",
                                    "description": "Usage or application of the chemical species e.g. metabolite",
                                },
                            },
                            "properties": {
                                "type": "array",
                                "items": {
                                    "type": "string",
                                    "description": "Criterion of a numerical property e.g. boiling point > 100°C",
                                },
                            },
                        },
                    },
                },
            },
        ]

    def get_name2method(self):
        return {
            "lookup_chemicalSpecies_attributes": self.lookup_chemicalSpecies_attributes,
            "find_chemicalSpecies": self.find_chemicalSpecies,
        }

    def _find_species_iri(self, species: str) -> List[str]:
        # TODO: look-up an in-memory cache of label to IRI mappings
        # TODO: use fuzzy matching
        if species in self._get_chemical_classes():
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

    def _get_chemicalSpecies_attribute(
        self, species_iri: str, attr_key: SpeciesAttrKey
    ) -> List[Dict[str, str]]:
        if isinstance(attr_key, SpeciesUseAttrKey):
            template = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>

SELECT DISTINCT ?Use WHERE {{
    <{IRI}> os:hasUse/rdfs:label ?Use .
}}"""
        elif isinstance(attr_key, SpeciesChemicalClassAttrKey):
            template = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>

SELECT DISTINCT ?ChemicalClass WHERE {{
    <{IRI}> (a|!a)+ [ a os:ChemicalClass ; rdfs:label ?ChemicalClass ] .
}}"""
        elif isinstance(attr_key, SpeciesIdentifierAttrKey):
            template = """PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>

SELECT DISTINCT ?{key} WHERE {{{{
    <{{IRI}}> os:has{key}/os:value ?{key} .
}}}}""".format(
                key=attr_key.value
            )
        else:
            template = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>

SELECT DISTINCT ?Value ?Unit ?ReferenceStateValue ?ReferenceStateUnit WHERE {{{{
    <{{IRI}}> os:has{key} ?{key} .
    ?{key} os:value ?Value ; os:unit/rdfs:label ?Unit .
    OPTIONAL {{{{
        ?{key} os:hasReferenceState [ os:value ?ReferenceStateValue ; os:unit/rdfs:label ?ReferenceStateUnit ] .
    }}}}
}}}}""".format(
                key=attr_key.value
            )
        query = template.format(IRI=species_iri)
        return [
            {k: v["value"] for k, v in x.items()}
            for x in self.kg_client.query(query)["results"]["bindings"]
        ]

    def lookup_chemicalSpecies_attributes(self, species: str, attributes: List[str]):
        logger.info("Aligning attribute keys: " + attributes)
        attr_keys: List[SpeciesAttrKey] = []
        for key in self.nn_retriever.retrieve(
            documents=self._SPECIES_ATTR_KEYS, queries=attributes
        ):
            for cls in self._SPECIES_ATTR_CLSES:
                try:
                    attr_key = cls(key)
                    attr_keys.append(attr_key)
                except ValueError:
                    pass
        logger.info("Aligned attribute keys: " + str(attr_keys))

        species_iris = self._find_species_iri(species)
        # TODO: do not include entries with empty attributes
        return [
            {
                **{"IRI": iri},
                **{
                    key.value: self._get_chemicalSpecies_attribute(iri, key)
                    for key in attr_keys
                },
            }
            for iri in species_iris
        ]

    def _find_chemicalSpecies_iri(
        self,
        chemical_classes: List[str] = [],
        uses: List[str] = [],
        properties: List[
            Tuple[SpeciesPropertyAttrKey, CompoundNumericalConstraint]
        ] = [],
    ) -> List[str]:
        patterns = []

        if chemical_classes:
            # TODO: instead of align each chemical_class to its closest neighbour, retrieve top-k 
            # and match to any one of these top-k labels
            logger.info("Aligning chemical class labels...")
            stored_chemical_classes = self._get_chemical_classes()
            chemical_classes = self.nn_retriever.retrieve(
                stored_chemical_classes, chemical_classes
            )
            logger.info("Aligned chemical classes: " + str(chemical_classes))
        for chemical_class in chemical_classes:
            patterns.append(
                '?Species (a|!a)+ [ a os:ChemicalClass ; rdfs:label "{label}" ] .'.format(
                    label=chemical_class
                )
            )
        for use in uses:
            patterns.append(
                '?Species os:hasUse/rdfs:label "{label}" .'.format(label=use)
            )
        for key, compound_constraint in properties:
            patterns.append(
                "?Species os:has{key}/os:value ?{key}Value .".format(key=key.value)
            )
            atomic_constraints = [
                "?{key}Value {operator} {operand}".format(
                    key=key.value, operator=x.operator, operand=x.operand
                )
                for x in compound_constraint.constraints
            ]
            if compound_constraint.logical_operator:
                exprn = compound_constraint.logical_operator.value.join(
                    atomic_constraints
                )
            else:
                exprn = atomic_constraints[0]
            patterns.append("FILTER ( {exprn} )".format(exprn=exprn))
        query = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>

SELECT ?Species WHERE {{
{patterns}
}}""".format(
            patterns="\n".join(patterns)
        )
        return [
            x["Species"]["value"]
            for x in self.kg_client.query(query)["results"]["bindings"]
        ]

    def _get_label(self, iri: str):
        query = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        
SELECT ?Label WHERE {{
    <{IRI}> rdfs:label ?Label
}}""".format(
            IRI=iri
        )
        bindings = self.kg_client.query(query)["results"]["bindings"]
        if not bindings:
            return None
        return bindings[0]["Label"]["value"]

    def find_chemicalSpecies(
        self,
        chemical_classes: List[str] = [],
        uses: List[str] = [],
        properties: List[str] = [],
    ):
        logger.info("Parsing property constraints...")
        property_constraints = [parse_constraint(x) for x in properties]
        aligned_keys = [
            SpeciesPropertyAttrKey(
                self.nn_retriever.retrieve(
                    documents=[x.value for x in SpeciesPropertyAttrKey],
                    queries=["".join([x.capitalize() for x in key.split()])],
                )[0]
            )
            for key, _ in property_constraints
        ]
        aligned_property_constraints = [
            (aligned_key, constraint)
            for aligned_key, (_, constraint) in zip(aligned_keys, property_constraints)
        ]
        logger.info("Parsed property constraints: " + str(aligned_property_constraints))

        species_iris = self._find_chemicalSpecies_iri(
            chemical_classes, uses, aligned_property_constraints
        )
        return [dict(IRI=iri, label=self._get_label(iri)) for iri in species_iris]
