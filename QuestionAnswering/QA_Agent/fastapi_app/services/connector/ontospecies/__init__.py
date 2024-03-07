import logging
import time
from typing import List, Type

import unit_parse
from .find_species import SpeciesFinder

from model.qa import QAData, QAStep
from services.func_call import get_func_caller
from services.utils.parse import ConstraintParser, SchemaParser
from services.nearest_neighbor import NNRetriever
from model.constraint import AtomicNumericalConstraint, CompoundNumericalConstraint
from fastapi_app.services.connector.agent_connector import IAgentConnector
from .constants import (
    SpeciesAttrKey,
    SpeciesChemicalClassAttrKey,
    SpeciesUseAttrKey,
    SpeciesIdentifierAttrKey,
    SpeciesPropertyAttrKey,
)
from .kg_client import get_ontospecies_kg_client
from .retrieve_attributes import SpeciesAttributeRetriever
from .store import get_ontospecies_literal_store

logger = logging.getLogger(__name__)


class OntoSpeciesAgentConnector(IAgentConnector):
    FUNCS = [
        {
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
        {
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
    ]

    _SPECIES_ATTR_CLSES: List[Type[SpeciesAttrKey]] = [
        SpeciesChemicalClassAttrKey,
        SpeciesUseAttrKey,
        SpeciesIdentifierAttrKey,
        SpeciesPropertyAttrKey,
    ]
    _SPECIES_ATTR_KEYS = [x.value for cls in _SPECIES_ATTR_CLSES for x in cls]

    def __init__(self, nn_retriever: NNRetriever):
        self.kg_client = get_ontospecies_kg_client()
        self.literal_store = get_ontospecies_literal_store()
        self.species_attr_retriever = SpeciesAttributeRetriever()
        self.species_finder = SpeciesFinder()
        self.nn_retriever = nn_retriever
        self.constraint_parser = ConstraintParser(
            schema_parser=SchemaParser(func_call_predictor=get_func_caller())
        )

    @classmethod
    def get_funcs(cls):
        return cls.FUNCS

    def get_name2method(self):
        return {
            "lookup_chemicalSpecies_attributes": self.lookup_chemicalSpecies_attributes,
            "find_chemicalSpecies": self.find_chemicalSpecies,
        }

    def lookup_chemicalSpecies_attributes(self, species: str, attributes: List[str]):
        steps: List[QAStep] = []

        logger.info("Aligning attribute keys: " + str(attributes))
        timestamp = time.time()
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
        latency = time.time() - timestamp
        logger.info("Aligned attribute keys: " + str(attr_keys))

        timestamp = time.time()
        species_iris = self.species_finder.find(species)
        latency = time.time() - timestamp
        steps.append(
            QAStep(
                latency=latency,
                action="find_species_iri",
                arguments=[dict(species=species)],
            )
        )

        timestamp = time.time()
        bindings: List[dict] = []
        for iri in species_iris:
            datum = dict(IRI=iri)
            for key in attr_keys:
                datum[key.value] = self.species_attr_retriever.retrieve(iri, key)
            bindings.append(datum)
        latency = time.time() - timestamp
        limit = 10
        attr_keys_unpacked = [key.value for key in attr_keys]
        arguments = [
            dict(IRI=iri, attributes=attr_keys_unpacked) for iri in species_iris[:limit]
        ]
        if len(species_iris) > limit:
            arguments.append("...")
        steps.append(
            QAStep(latency=latency, action="lookup_attributes", arguments=arguments)
        )
        return steps, QAData(vars=["IRI"] + attr_keys_unpacked, bindings=bindings)

    def find_chemicalSpecies(
        self,
        chemical_classes: List[str] = [],
        uses: List[str] = [],
        properties: List[str] = [],
    ):
        steps = []

        if chemical_classes:
            # TODO: instead of align each chemical_class to its closest neighbour, retrieve top-k
            # and match to any one of these top-k labels
            logger.info("Aligning chemical class labels...")
            abox_chemical_classes = self.literal_store.get_chemical_classes()
            chemical_classes = self.nn_retriever.retrieve(
                abox_chemical_classes, chemical_classes
            )
            logger.info("Aligned chemical classes: " + str(chemical_classes))

        if uses:
            # TODO: instead of align each use to its closest neighbour, retrieve top-k
            # and match to any one of these top-k labels
            logger.info("Aligning use labels...")
            abox_uses = self.literal_store.get_uses()
            uses = self.nn_retriever.retrieve(abox_uses, uses)
            logger.info("Aligned uses: " + str(uses))

        logger.info("Parsing property constraints...")
        property_constraints = [self.constraint_parser.parse(x) for x in properties]

        property_constraints_unit_converted = []
        for _, compound_constraint in property_constraints:
            atomic_constraints = []
            for constraint in compound_constraint.constraints:
                operand = constraint.operand
                unit = constraint.unit
                if constraint.unit:
                    try:
                        quantity = unit_parse.parser(
                            " ".join([str(constraint.operand), constraint.unit])
                        )
                        while isinstance(quantity, list):
                            quantity = quantity[0]
                        quantity = quantity.to_base_units()  # meter/kg/second
                        if str(quantity.units) == "kg / mol":
                            quantity = quantity.to_root_units()  # meter/gram/second
                        unit = str(quantity.units)
                        operand = quantity.magnitude
                    except:
                        pass
                atomic_constraints.append(
                    AtomicNumericalConstraint(
                        operator=constraint.operator, operand=operand, unit=unit
                    )
                )
            property_constraints_unit_converted.append(
                CompoundNumericalConstraint(
                    logical_operator=compound_constraint.logical_operator,
                    constraints=atomic_constraints,
                )
            )

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
            for aligned_key, constraint in zip(
                aligned_keys, property_constraints_unit_converted
            )
        ]
        logger.info("Parsed property constraints: " + str(aligned_property_constraints))

        timestamp = time.time()
        species_iris = self._find_chemicalSpecies(
            chemical_classes, uses, aligned_property_constraints
        )
        bindings = [
            dict(IRI=iri, label=self.literal_store.get_label(iri))
            for iri in species_iris
        ]
        latency = time.time() - timestamp
        arguments = dict()
        if chemical_classes:
            arguments["chemical_classes"] = chemical_classes
        if uses:
            arguments["uses"] = uses
        if aligned_property_constraints:
            arguments["properties"] = [
                dict(property=key.value, constraint=str(constraint))
                for key, constraint in aligned_property_constraints
            ]
        steps.append(
            QAStep(
                action="find_chemicalSpecies", arguments=[arguments], latency=latency
            )
        )

        return steps, QAData(
            vars=["IRI", "label"],
            bindings=bindings,
        )
