import logging
import time
from typing import Annotated, List, Optional

from fastapi import Depends

from model.qa import QAStep
from services.core.parse import KeyAggregateParser
from services.core.align_enum import EnumAligner
from services.connectors.agent_connector import AgentConnectorBase
from .constants import FactoryAttrKey, FactoryConcept
from .align import get_factory_attr_key_aligner, get_factory_concept_aligner
from .parse import get_factory_attr_agg_parser
from .agent import SGFactoriesAgent, get_sg_factories_agent


logger = logging.getLogger(__name__)


class SGFactoriesAgentConnector(AgentConnectorBase):
    def __init__(
        self,
        agent: SGFactoriesAgent,
        factory_attr_key_aligner: EnumAligner[FactoryAttrKey],
        factory_concept_aligner: EnumAligner[FactoryConcept],
        attr_agg_parser: KeyAggregateParser[FactoryAttrKey],
    ):
        self.agent = agent
        self.factory_attr_key_aligner = factory_attr_key_aligner
        self.factory_concept_aligner = factory_concept_aligner
        self.attr_agg_parser = attr_agg_parser

    @property
    def funcs(self):
        return [
            {
                "name": "lookup_factory_attribute",
                "description": "Retrieve a data attribute of the given factory",
                "parameters": {
                    "type": "object",
                    "properties": {
                        "plant_name": {
                            "type": "string",
                            "description": "Name of the factory",
                        },
                        "attribute": {
                            "type": "string",
                            "description": "Name of a factory attribute e.g. industry, heat emission, specific energy consumption",
                        },
                    },
                },
                "required": ["plant_name", "attribute"],
            },
            {
                "name": "count_factories",
                "description": "Count the number of factories",
                "parameters": {
                    "type": "object",
                    "properties": {
                        "factory_type": {
                            "type": "string",
                            "description": "Factory category e.g. chemical plant, food plant",
                        },
                        "groupby_type": {
                            "type": "boolean",
                            "description": "If true, count factories for each category",
                        },
                    },
                },
            },
            {
                "name": "compute_aggregate_factory_attribute",
                "description": "Aggregate attribute data of factories",
                "parameters": {
                    "type": "object",
                    "properties": {
                        "factory_type": {
                            "type": "string",
                            "description": "Factory category e.g. chemical plant, food plant",
                        },
                        "attribute_aggregate": {
                            "type": "string",
                            "description": "Aggregate of an attribute value e.g. total heat emission, lowest design capacity",
                        },
                        "groupby_type": {
                            "type": "boolean",
                            "description": "If true, aggregate data for each factory category",
                        },
                    },
                },
                "required": ["attribute_aggregate"],
            },
        ]

    @property
    def name2method(
        self,
    ):
        return {
            "lookup_factory_attribute": self.lookup_factory_attribute,
            "count_factories": self.count_factories,
            "compute_aggregate_factory_attribute": self.compute_aggregate_factory_attribute,
        }

    def lookup_factory_attribute(self, plant_name: str, attribute: str):
        steps: List[QAStep] = []

        logger.info("Aligning attribute: " + attribute)
        timestamp = time.time()
        attr_key = self.factory_attr_key_aligner.align(attribute)
        latency = time.time() - timestamp
        logger.info("Aligned attribute: " + str(attr_key))
        steps.append(
            QAStep(
                action="align_attribute_key",
                arguments=attribute,
                results=attr_key.value,
                latency=latency,
            )
        )

        timestamp = time.time()
        data = self.agent.lookup_factory_attribute(
            plant_name=plant_name, attr_key=attr_key
        )
        latency = time.time() - timestamp
        steps.append(
            QAStep(
                action="lookup_factory_attribute",
                arguments=dict(plant_name=plant_name, attr_key=attr_key.value),
                latency=latency,
            )
        )

        return steps, data

    def _align_factory_concept(self, factory_type: Optional[str]):
        if factory_type:
            logger.info("Aligning factory type: " + factory_type)
            timestamp = time.time()
            factory_concept = self.factory_concept_aligner.align(factory_type)
            latency = time.time() - timestamp
            logger.info("Aligned factory type: " + str(factory_concept))
            step = QAStep(
                action="align_factory_type",
                arguments=factory_type,
                results=factory_concept.value,
                latency=latency,
            )
        else:
            factory_concept = None
            step = None
        return step, factory_concept

    def count_factories(
        self, factory_type: Optional[str] = None, groupby_type: bool = False
    ):
        steps: List[QAStep] = []
        step, factory_concept = self._align_factory_concept(factory_type)
        if step:
            steps.append(step)

        timestamp = time.time()
        data = self.agent.count_factories(
            factory_type=factory_concept, groupby_type=groupby_type
        )
        latency = time.time() - timestamp
        steps.append(
            QAStep(
                action="count_factories",
                arguments=dict(
                    factory_type=factory_concept.value, groupby_type=groupby_type
                ),
                latency=latency,
            )
        )

        return steps, data

    def compute_aggregate_factory_attribute(
        self,
        attribute_aggregate: str,
        factory_type: Optional[str] = None,
        groupby_type: bool = False,
    ):
        steps: List[QAStep] = []
        step, factory_concept = self._align_factory_concept(factory_type)
        if step:
            steps.append(step)

        logger.info("Aligning attribute aggregate: " + attribute_aggregate)
        timestamp = time.time()
        attr_agg = self.attr_agg_parser.parse(attribute_aggregate)
        latency = time.time() - timestamp
        unpacked_attr_agg = tuple(x.value for x in attr_agg)
        steps.append(
            QAStep(
                action="align_attr_agg",
                arguments=attribute_aggregate,
                results=unpacked_attr_agg,
                latency=latency,
            )
        )

        timestamp = time.time()
        data = self.agent.compute_aggregate_factory_attribute(
            factory_type=factory_concept, attr_agg=attr_agg, groupby_type=groupby_type
        )
        latency = time.time() - timestamp
        steps.append(
            QAStep(
                action="compute_aggregate_factory_attribute",
                arguments=dict(
                    factory_type=factory_concept.value if factory_concept else None,
                    attr_agg=unpacked_attr_agg,
                    groupby_type=groupby_type,
                ),
                latency=latency,
            )
        )

        return steps, data


def get_sg_factories_agent_connector(
    agent: Annotated[SGFactoriesAgent, Depends(get_sg_factories_agent)],
    factory_attr_key_aligner: Annotated[
        EnumAligner[FactoryAttrKey], Depends(get_factory_attr_key_aligner)
    ],
    factory_concept_aligner: Annotated[
        EnumAligner[FactoryConcept], Depends(get_factory_concept_aligner)
    ],
    attr_agg_parser: Annotated[
        KeyAggregateParser[FactoryAttrKey], Depends(get_factory_attr_agg_parser)
    ],
):
    return SGFactoriesAgentConnector(
        agent=agent,
        factory_attr_key_aligner=factory_attr_key_aligner,
        factory_concept_aligner=factory_concept_aligner,
        attr_agg_parser=attr_agg_parser,
    )
