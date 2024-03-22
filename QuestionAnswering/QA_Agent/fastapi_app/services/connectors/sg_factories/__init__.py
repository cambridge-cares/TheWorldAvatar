import logging
import time
from typing import Annotated, List, Optional

from fastapi import Depends

from model.qa import QAStep
from services.core.parse import KeyAggregateParser
from services.core.align_enum import EnumAligner
from services.connectors.agent_connector import AgentConnectorBase
from .model import FactoryIndustryKey, FactoryNumAttrKey, Industry
from .align import (
    get_factoryIndustryKey_aligner,
    get_factoryNumAttrkey_aligner,
    get_industry_aligner,
)
from .parse import (
    FactoryConstraintsParser,
    get_factoryAttr_aggParser,
    get_factoryConstraints_parser,
)
from .agent import SGFactoriesAgent, get_sgFactories_agent


logger = logging.getLogger(__name__)


class SGFactoriesAgentConnector(AgentConnectorBase):
    def __init__(
        self,
        agent: SGFactoriesAgent,
        factory_industry_key_aligner: EnumAligner[FactoryIndustryKey],
        factory_num_attr_key_aligner: EnumAligner[FactoryNumAttrKey],
        industry_aligner: EnumAligner[Industry],
        attr_agg_parser: KeyAggregateParser[FactoryNumAttrKey],
        factory_constraints_parser: FactoryConstraintsParser,
    ):
        self.agent = agent
        # TODO: Implement an aligner for factory attribute keys, index whether the attribute is numerical or categorical
        self.factory_industry_key_aligner = factory_industry_key_aligner
        self.factory_num_attr_key_aligner = factory_num_attr_key_aligner
        self.industry_aligner = industry_aligner
        self.attr_agg_parser = attr_agg_parser
        self.factory_constraints_parser = factory_constraints_parser

    @property
    def funcs(self):
        return [
            {
                "name": "lookup_factory_attribute",
                "description": "Retrieve a data attribute of the given factory (aka manufacturng plant or facility)",
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
                "description": "Find factories (aka manufacturing plants or facilities) that satisfy some constraints",
                "parameters": {
                    "type": "object",
                    "properties": {
                        "industry": {
                            "type": ["string", "null"],
                            "description": "Industry aka sector to search in e.g. chemical; if null, search across all industries",
                        },
                        "numerical_constraints": {
                            "type": "string",
                            "description": "Numerical constraints e.g. lowest heat emission, greatest production rate",
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
                "description": "Count the number of factories (aka manufacturing plants or facilities)",
                "parameters": {
                    "type": "object",
                    "properties": {
                        "industry": {
                            "type": ["string", "null"],
                            "description": "Industry aka sector to count in e.g. food; if null, count for every industry",
                        }
                    },
                },
            },
            {
                "name": "compute_aggregate_factory_attribute",
                "description": "Aggregate attribute data of factories (aka manufacturing plants or facilities)",
                "parameters": {
                    "type": "object",
                    "properties": {
                        "attribute_aggregate": {
                            "type": "string",
                            "description": "An attribute name and an aggregate operation acted upon it e.g. total heat emission, lowest production volume",
                        },
                        "industry": {
                            "type": ["string", "null"],
                            "description": "Industry aka sector to aggregate over e.g. semiconductor; if null, aggregate across each industry",
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
            "find_factories": self.find_factories,
            "count_factories": self.count_factories,
            "compute_aggregate_factory_attribute": self.compute_aggregate_factory_attribute,
        }

    def lookup_factory_attribute(self, plant_name: str, attribute: str):
        steps: List[QAStep] = []

        logger.info("Aligning attribute: " + attribute)
        timestamp = time.time()

        industry_key, industry_score = (
            self.factory_industry_key_aligner.align_with_score(attribute)
        )
        num_attr_key, num_attr_score = (
            self.factory_num_attr_key_aligner.align_with_score(attribute)
        )

        if industry_score > num_attr_score:
            attr_key = industry_key
        else:
            attr_key = num_attr_key

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
        if not industry:
            return None, None
        
        logger.info("Aligning factory type: " + industry)
        timestamp = time.time()
        aligned_industry = self.industry_aligner.align(industry)
        latency = time.time() - timestamp
        logger.info("Aligned factory type: " + str(aligned_industry))
        step = QAStep(
            action="align_factory_type",
            arguments=industry,
            results=aligned_industry.value,
            latency=latency,
        )
        
        return step, aligned_industry

    def find_factories(
        self,
        industry: Optional[str] = None,
        numerical_constraints: Optional[str] = None,
        limit: Optional[int] = None,
    ):
        steps: List[QAStep] = []

        step, aligned_industry = self._align_industry(industry)
        if step:
            steps.append(step)

        logger.info("Parsing factory constraints: " + numerical_constraints)
        timestamp = time.time()
        parsed_constraints = self.factory_constraints_parser.parse(
            numerical_constraints
        )
        latency = time.time() - timestamp
        logger.info("Parsed factory constraints: " + str(parsed_constraints))
        steps.append(
            QAStep(
                action="parse_factory_constraint",
                arguments=numerical_constraints,
                results=str(parsed_constraints),
                latency=latency,
            )
        )

        timestamp = time.time()
        data = self.agent.find_factories(
            industry=aligned_industry, numattr_constraints=parsed_constraints, limit=limit
        )
        latency = time.time() - timestamp
        steps.append(
            QAStep(
                action="find_factories",
                arguments=dict(constraints=str(numerical_constraints), limit=limit),
                latency=latency,
            )
        )

        return steps, data

    def count_factories(self, industry: Optional[str] = None):
        steps: List[QAStep] = []
        step, aligned_industry = self._align_industry(industry)
        if step:
            steps.append(step)

        timestamp = time.time()
        data = self.agent.count_factories(industry=aligned_industry)
        latency = time.time() - timestamp
        steps.append(
            QAStep(
                action="count_factories",
                arguments=dict(
                    industry=aligned_industry.value if aligned_industry else None,
                ),
                latency=latency,
            )
        )

        return steps, data

    def compute_aggregate_factory_attribute(
        self,
        attribute_aggregate: str,
        industry: Optional[str] = None,
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
        )
        latency = time.time() - timestamp
        steps.append(
            QAStep(
                action="compute_aggregate_factory_attribute",
                arguments=dict(
                    attr_agg=unpacked_attr_agg,
                    industry=aligned_industry.value if aligned_industry else None,
                ),
                latency=latency,
            )
        )

        return steps, data


def get_sgFactories_agentConnector(
    agent: Annotated[SGFactoriesAgent, Depends(get_sgFactories_agent)],
    factory_industry_key_aligner: Annotated[
        EnumAligner[FactoryIndustryKey], Depends(get_factoryIndustryKey_aligner)
    ],
    factory_num_attr_key_aligner: Annotated[
        EnumAligner[FactoryNumAttrKey], Depends(get_factoryNumAttrkey_aligner)
    ],
    industry_aligner: Annotated[EnumAligner[Industry], Depends(get_industry_aligner)],
    attr_agg_parser: Annotated[
        KeyAggregateParser[FactoryNumAttrKey], Depends(get_factoryAttr_aggParser)
    ],
    factory_constraints_parser: Annotated[
        FactoryConstraintsParser, Depends(get_factoryConstraints_parser)
    ],
):
    return SGFactoriesAgentConnector(
        agent=agent,
        factory_industry_key_aligner=factory_industry_key_aligner,
        factory_num_attr_key_aligner=factory_num_attr_key_aligner,
        industry_aligner=industry_aligner,
        attr_agg_parser=attr_agg_parser,
        factory_constraints_parser=factory_constraints_parser,
    )
