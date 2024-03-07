import logging
import time
from typing import List, Type

import unit_parse

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
from .store import get_ontospecies_literal_store
from .agent import OntoSpeciesAgent

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
        self.agent = OntoSpeciesAgent()
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

    def _align_attribute_keys(self, attributes: List[str]):
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
        return attr_keys

    def lookup_chemicalSpecies_attributes(self, species: str, attributes: List[str]):
        steps: List[QAStep] = []

        logger.info("Aligning attribute keys: " + str(attributes))
        timestamp = time.time()
        attr_keys = self._align_attribute_keys(attributes)
        latency = time.time() - timestamp
        logger.info("Aligned attribute keys: " + str(attr_keys))
        steps.append(
            QAStep(action="align_attribute_keys", arguments=attributes, latency=latency)
        )

        timestamp = time.time()
        bindings = self.agent.lookup_chemicalSpecies_attributes(species, attr_keys)
        latency = time.time() - timestamp
        steps.append(
            QAStep(
                latency=latency,
                action="lookup_attributes",
                arguments=dict(
                    species=species, attributes=[key.value for key in attr_keys]
                ),
            )
        )

        return steps, QAData(vars=list(bindings[0].keys()), bindings=bindings)

    def _align_chemical_classes(self, chemical_classes: List[str]):
        # TODO: instead of align each chemical_class to its closest neighbour, retrieve top-k
        # and match to any one of these top-k labels
        abox_chemical_classes = self.literal_store.get_chemical_classes()
        return self.nn_retriever.retrieve(
            abox_chemical_classes, chemical_classes
        )
    
    def _align_uses(self, uses: List[str]):
        # TODO: instead of align each use to its closest neighbour, retrieve top-k
        # and match to any one of these top-k labels
        abox_uses = self.literal_store.get_uses()
        return self.nn_retriever.retrieve(abox_uses, uses)

    def _parse_property_constraints(self, properties: List[str]):
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
        return [
            (aligned_key, constraint)
            for aligned_key, constraint in zip(
                aligned_keys, property_constraints_unit_converted
            )
        ]

    def find_chemicalSpecies(
        self,
        chemical_classes: List[str] = [],
        uses: List[str] = [],
        properties: List[str] = [],
    ):
        steps = []

        if chemical_classes:
            logger.info("Aligning chemical class labels...")
            timestamp = time.time()
            chemical_classes = self._align_chemical_classes(chemical_classes)
            latency = time.time() - timestamp
            logger.info("Aligned chemical classes: " + str(chemical_classes))
            steps.append(QAStep(action="align_chemical_classes", arguments=chemical_classes, latency=latency))

        if uses:
            logger.info("Aligning use labels...")
            timestamp = time.time()
            uses = self._align_uses(uses)
            latency = time.time() - timestamp
            logger.info("Aligned uses: " + str(uses))
            steps.append(QAStep(action="align_uses", arguments=uses, latency=latency))

        if properties:
            logger.info("Parsing property constraints...")
            property_constraints = self._parse_property_constraints(properties)
            logger.info("Parsed property constraints: " + str(property_constraints))
        else:
            property_constraints = []

        timestamp = time.time()
        species_iris = self.agent.find_chemicalSpecies(
            chemical_classes, uses, property_constraints
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
        if property_constraints:
            arguments["properties"] = [
                dict(property=key.value, constraint=str(constraint))
                for key, constraint in property_constraints
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
