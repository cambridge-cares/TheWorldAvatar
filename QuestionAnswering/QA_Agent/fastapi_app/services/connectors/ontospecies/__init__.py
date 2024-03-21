from functools import cached_property
import logging
import time
from typing import Annotated, List
from fastapi import Depends

from redis import Redis
import unit_parse

from model.qa import QAStep
from model.constraint import AtomicNumericalConstraint, CompoundNumericalConstraint
from services.core.redis import get_redis_client
from services.core.embed import IEmbedder, get_embedder
from services.core.kg import KgClient
from services.core.retrieve_docs import DocsRetriever
from services.connectors.agent_connector import AgentConnectorBase
from .constants import (
    SpeciesAttrKey,
    SpeciesChemicalClassAttrKey,
    SpeciesIdentifierAttrKey,
    SpeciesPropertyAttrKey,
    SpeciesUseAttrKey,
)
from .kg import get_ontospecies_kg_client
from .parse import ConstraintParser, get_constraint_parser
from .agent import OntoSpeciesAgent, get_ontospecies_agent

logger = logging.getLogger(__name__)


class OntoSpeciesAgentConnector(AgentConnectorBase):
    def __init__(
        self,
        embedder: IEmbedder,
        redis_client: Redis,
        kg_client: KgClient,
        ontospecies_agent: OntoSpeciesAgent,
        constraint_parser: ConstraintParser,
    ):
        self.kg_client = kg_client
        self.agent = ontospecies_agent
        # TODO: Create a custom OntoSpeciesAttributeKeyRetriever class that indexes 'value' and 'superclass'
        self.os_attribute_keys_retriever = DocsRetriever(
            embedder=embedder,
            redis_client=redis_client,
            key="ontospecies:attribute_keys",
            docs=[
                x.value
                for cls in (
                    SpeciesChemicalClassAttrKey,
                    SpeciesUseAttrKey,
                    SpeciesIdentifierAttrKey,
                    SpeciesPropertyAttrKey,
                )
                for x in cls
            ],
        )
        self.os_property_keys_retriever = DocsRetriever(
            embedder=embedder,
            redis_client=redis_client,
            key="ontospecies:property_keys",
            docs=[x.value for x in SpeciesPropertyAttrKey],
        )
        self.constraint_parser = constraint_parser

    @cached_property
    def funcs(self):
        return [
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
                                "description": "Name of chemical class e.g. aldehyde, alcohol",
                            },
                        },
                        "uses": {
                            "type": "array",
                            "items": {
                                "type": "string",
                                "description": "Usage or application e.g. solvent",
                            },
                        },
                        "numerical_properties": {
                            "type": "array",
                            "items": {
                                "type": "string",
                                "description": "Physical quantity e.g. boiling point between 100°C and 120°C",
                            },
                        },
                    },
                },
                "required": ["chemical_classes", "uses", "numerical_properties"],
            },
        ]

    @cached_property
    def name2method(self):
        return {
            "lookup_chemicalSpecies_attributes": self.lookup_chemicalSpecies_attributes,
            "find_chemicalSpecies": self.find_chemicalSpecies,
        }

    def _align_attribute_keys(self, attributes: List[str]):
        queries = [
            "".join([w.capitalize() for w in attr.split()]) for attr in attributes
        ]
        docs_scores_lst = self.os_attribute_keys_retriever.retrieve(
            queries=queries,
            k=1,
        )
        closest_neighs = [docs_scores[0][0] for docs_scores in docs_scores_lst]
        results: List[SpeciesAttrKey] = []
        for k in closest_neighs:
            for cls in self._SPECIES_ATTR_CLSES:
                try:
                    results.append(cls(k))
                    break
                except:
                    pass
        return results

    def lookup_chemicalSpecies_attributes(self, species: str, attributes: List[str]):
        steps: List[QAStep] = []

        logger.info("Aligning attribute keys: " + str(attributes))
        timestamp = time.time()
        attr_keys = self._align_attribute_keys(attributes)
        latency = time.time() - timestamp
        logger.info("Aligned attribute keys: " + str(attr_keys))
        steps.append(
            QAStep(
                action="align_attribute_keys",
                arguments=attributes,
                results=[x.value for x in attr_keys],
                latency=latency,
            )
        )

        timestamp = time.time()
        data = self.agent.lookup_chemicalSpecies_attributes(species, attr_keys)
        latency = time.time() - timestamp
        steps.append(
            QAStep(
                action="lookup_attributes",
                arguments=dict(
                    species=species, attributes=[key.value for key in attr_keys]
                ),
                latency=latency,
            )
        )

        return steps, data

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

    def _align_property_keys(self, properties: List[str]):
        queries = [
            "".join([w.capitalize() for w in prop.split()]) for prop in properties
        ]
        docs_scores_lst = self.os_property_key_retriever.retrieve(
            queries=queries,
            k=1,
        )
        return [
            SpeciesPropertyAttrKey(docs_scores[0][0]) for docs_scores in docs_scores_lst
        ]

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
        aligned_keys = self._align_property_keys(
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
        numerical_properties: List[str] = [],
    ):
        steps = []

        if numerical_properties:
            logger.info("Parsing property constraints...")
            timestamp = time.time()
            property_constraints = self._parse_property_constraints(
                numerical_properties
            )
            latency = time.time() - timestamp
            logger.info("Parsed property constraints: " + str(property_constraints))
            steps.append(
                QAStep(
                    action="align_property_constraints",
                    arguments=numerical_properties,
                    results=[
                        dict(key=key, constraint=str(constraint))
                        for key, constraint in property_constraints
                    ],
                    latency=latency,
                )
            )
        else:
            property_constraints = []

        timestamp = time.time()
        data = self.agent.find_chemicalSpecies(
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
            QAStep(action="find_chemicalSpecies", arguments=arguments, latency=latency)
        )

        return steps, data


def get_ontospecies_agent_connector(
    embedder: Annotated[IEmbedder, Depends(get_embedder)],
    redis_client: Annotated[Redis, Depends(get_redis_client)],
    kg_client: Annotated[KgClient, Depends(get_ontospecies_kg_client)],
    ontospecies_agent: Annotated[OntoSpeciesAgent, Depends(get_ontospecies_agent)],
    constraint_parser: Annotated[ConstraintParser, Depends(get_constraint_parser)],
):
    return OntoSpeciesAgentConnector(
        embedder=embedder,
        redis_client=redis_client,
        kg_client=kg_client,
        ontospecies_agent=ontospecies_agent,
        constraint_parser=constraint_parser,
    )
