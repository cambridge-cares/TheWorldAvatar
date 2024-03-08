import logging
import time
from typing import Annotated, List
from fastapi import Depends

import unit_parse

from model.qa import QAData, QAStep
from services.kg_client import KgClient
from services.utils.parse import ConstraintParser, get_constraint_parser
from model.constraint import AtomicNumericalConstraint, CompoundNumericalConstraint
from services.connector.agent_connector import IAgentConnector
from .kg_client import get_ontospecies_kg_client
from .agent import OntoSpeciesAgent, get_ontospecies_agent
from .align import OntoSpeciesAligner, get_ontospecies_aligner

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

    def __init__(
        self,
        kg_client: KgClient,
        ontospecies_agent: OntoSpeciesAgent,
        ontospecies_aligner: OntoSpeciesAligner,
        constraint_parser: Annotated[ConstraintParser, Depends(get_constraint_parser)],
    ):
        self.kg_client = kg_client
        self.agent = ontospecies_agent
        self.aligner = ontospecies_aligner
        self.constraint_parser = constraint_parser

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
        attr_keys = self.aligner.align_attribute_keys(attributes)
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

    def _convert_units(self, constraint: AtomicNumericalConstraint):
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
        return AtomicNumericalConstraint(
            operator=constraint.operator, operand=operand, unit=unit
        )

    def _parse_property_constraints(self, properties: List[str]):
        property_constraints = [self.constraint_parser.parse(x) for x in properties]

        property_constraints_unit_converted = [
            CompoundNumericalConstraint(
                logical_operator=compound_constraint.logical_operator,
                constraints=[
                    self._convert_units(constraint)
                    for constraint in compound_constraint.constraints
                ],
            )
            for _, compound_constraint in property_constraints
        ]
        aligned_keys = self.aligner.align_property_keys(
            [key for key, _ in property_constraints]
        )

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

        if properties:
            logger.info("Parsing property constraints...")
            timestamp = time.time()
            property_constraints = self._parse_property_constraints(properties)
            latency = time.time() - timestamp
            logger.info("Parsed property constraints: " + str(property_constraints))
            steps.append(
                QAStep(
                    action="align_property_constraints",
                    arguments=properties,
                    latency=latency,
                )
            )
        else:
            property_constraints = []

        timestamp = time.time()
        bindings = self.agent.find_chemicalSpecies(
            chemical_classes, uses, property_constraints
        )
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


def get_ontospecies_agent_connector_getter(
    kg_client: Annotated[KgClient, Depends(get_ontospecies_kg_client)],
    ontospecies_agent: Annotated[OntoSpeciesAgent, Depends(get_ontospecies_agent)],
    ontospecies_aligner: Annotated[
        OntoSpeciesAligner, Depends(get_ontospecies_aligner)
    ],
    constraint_parser: Annotated[ConstraintParser, Depends(get_constraint_parser)],
):
    return lambda: OntoSpeciesAgentConnector(
        kg_client=kg_client,
        ontospecies_agent=ontospecies_agent,
        ontospecies_aligner=ontospecies_aligner,
        constraint_parser=constraint_parser,
    )
