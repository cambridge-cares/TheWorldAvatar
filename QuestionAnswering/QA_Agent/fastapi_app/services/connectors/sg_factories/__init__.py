import logging
import time
from typing import Annotated, List, Optional

from fastapi import Depends

from model.qa import QAStep
from services.core.parse import KeyAggregateParser
from services.core.align_enum import EnumAligner
from services.connectors.agent_connector import AgentConnectorBase
from .model import FactoryAttrKey, Industry
from .align import get_factory_attr_key_aligner, get_industry_aligner
from .parse import FactoryConstraintsParser, get_factory_attr_agg_parser
from .agent import SGFactoriesAgent, get_sg_factories_agent


logger = logging.getLogger(__name__)


class SGFactoriesAgentConnector(AgentConnectorBase):
    def __init__(
        self,
        agent: SGFactoriesAgent,
        factory_attr_key_aligner: EnumAligner[FactoryAttrKey],
        industry_aligner: EnumAligner[Industry],
        attr_agg_parser: KeyAggregateParser[FactoryAttrKey],
        factory_constraints_parser: FactoryConstraintsParser,
    ):
        self.agent = agent
        self.factory_attr_key_aligner = factory_attr_key_aligner
        self.industry_aligner = industry_aligner
        self.attr_agg_parser = attr_agg_parser
        self.factory_constraints_parser = factory_constraints_parser

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
                "name": "find_factories",
                "description": "Find factories that satisfy some constraints",
                "parameters": {
                    "type": "object",
                    "properties": {
                        "constraints": {
                            "type": "string",
                            "description": "Constraints on factories to find e.g. belonging to chemical industry with lowest heat emission",
                        },
                        "limit": {
                            "type": ["number", "null"],
                            "description": "Number of search results to return; if null, return all factories that match the criteria",
                        },
                    },
                },
            },
            {
                "name": "count_factories",
                "description": "Count the number of factories",
                "parameters": {
                    "type": "object",
                    "properties": {
                        "industry": {
                            "type": "string",
                            "description": "Limit the counting to a particular industry e.g. chemical industry",
                        },
                        "groupby_industry": {
                            "type": "boolean",
                            "description": "If true, count factories for each industry",
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
                        "attribute_aggregate": {
                            "type": "string",
                            "description": "Aggregate of an attribute value e.g. total heat emission, lowest design capacity",
                        },
                        "industry": {
                            "type": "string",
                            "description": "Industry aka sector over which aggregation is performed e.g. chemical industry",
                        },
                        "groupby_industry": {
                            "type": "boolean",
                            "description": "If true, aggregate data for each industry",
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

    def _align_industry(self, industry: Optional[str]):
        if industry:
            logger.info("Aligning factory type: " + industry)
            timestamp = time.time()
            factory_concept = self.industry_aligner.align(industry)
            latency = time.time() - timestamp
            logger.info("Aligned factory type: " + str(factory_concept))
            step = QAStep(
                action="align_factory_type",
                arguments=industry,
                results=factory_concept.value,
                latency=latency,
            )
        else:
            factory_concept = None
            step = None
        return step, factory_concept

    def find_factories(
        self, constraints: Optional[str] = None, limit: Optional[int] = None
    ):
        steps: List[QAStep] = []

        logger.info("Parsing factory constraints: " + constraints)
        timestamp = time.time()
        parsed_constraints = self.factory_constraints_parser.parse(constraints)
        latency = time.time() - timestamp
        logger.info("Parsed factory constraints: " + str(parsed_constraints))
        steps.append(
            QAStep(
                action="parse_factory_constraint",
                arguments=constraints,
                results=str(parsed_constraints),
                latency=latency,
            )
        )

        timestamp = time.time()
        data = self.agent.find_factories(constraints=constraints, limit=limit)
        latency = time.time() - timestamp
        steps.append(
            QAStep(
                action="find_factories",
                arguments=dict(constraints=str(constraints), limit=limit),
                latency=latency,
            )
        )

        return steps, data

    def count_factories(
        self, industry: Optional[str] = None, groupby_industry: bool = False
    ):
        steps: List[QAStep] = []
        step, aligned_industry = self._align_industry(industry)
        if step:
            steps.append(step)

        timestamp = time.time()
        data = self.agent.count_factories(
            industry=aligned_industry, groupby_industry=groupby_industry
        )
        latency = time.time() - timestamp
        steps.append(
            QAStep(
                action="count_factories",
                arguments=dict(
                    industry=aligned_industry.value, groupby_industry=groupby_industry
                ),
                latency=latency,
            )
        )

        return steps, data

    def compute_aggregate_factory_attribute(
        self,
        attribute_aggregate: str,
        industry: Optional[str] = None,
        groupby_industry: bool = False,
    ):
        steps: List[QAStep] = []

        step, aligned_industry = self._align_industry(industry)
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
            attr_agg=attr_agg,
            industry=aligned_industry,
            groupby_industry=groupby_industry,
        )
        latency = time.time() - timestamp
        steps.append(
            QAStep(
                action="compute_aggregate_factory_attribute",
                arguments=dict(
                    attr_agg=unpacked_attr_agg,
                    industry=aligned_industry.value if aligned_industry else None,
                    groupby_industry=groupby_industry,
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
    industry_aligner: Annotated[EnumAligner[Industry], Depends(get_industry_aligner)],
    attr_agg_parser: Annotated[
        KeyAggregateParser[FactoryAttrKey], Depends(get_factory_attr_agg_parser)
    ],
):
    return SGFactoriesAgentConnector(
        agent=agent,
        factory_attr_key_aligner=factory_attr_key_aligner,
        industry_aligner=industry_aligner,
        attr_agg_parser=attr_agg_parser,
    )
