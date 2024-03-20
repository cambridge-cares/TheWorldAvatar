from functools import cached_property
import logging
import time
from typing import Annotated, List

from fastapi import Depends

from model.qa import QAStep
from services.core.align_enum import EnumAligner
from services.core.parse import KeyAggregateParser
from services.connectors.agent_connector import AgentConnectorBase
from .constants import PlotAttrKey
from .agent import SGLandLotsAgent, get_sg_land_lots_agent
from .parse import (
    PlotConstraintsParser,
    get_plot_attr_agg_parser,
    get_plot_constraint_parser,
)
from .align import get_plot_attr_key_aligner

logger = logging.getLogger(__name__)


class SGLandLotsAgentConnector(AgentConnectorBase):
    def __init__(
        self,
        plot_attr_key_aligner: EnumAligner[PlotAttrKey],
        plot_constraints_parser: PlotConstraintsParser,
        agent: SGLandLotsAgent,
        attr_agg_parser: KeyAggregateParser[PlotAttrKey],
    ):
        self.plot_constraints_parser = plot_constraints_parser
        self.agent = agent
        self.plot_attr_key_aligner = plot_attr_key_aligner
        self.attr_agg_parser = attr_agg_parser

    @cached_property
    def funcs(self):
        return [
            {
                "name": "lookup_plot_attribute",
                "description": "Look up an attribute of a subset of plots satisfying some constraints",
                "parameters": {
                    "type": "object",
                    "properties": {
                        "plot_constraints": {
                            "type": "string",
                            "description": "Conditions that define the plots of interest e.g. plots for commercial usage with gross floor area less than 1000sqm",
                        },
                        "attribute": {
                            "type": "string",
                            "description": "Attribute to query e.g. land use type, gross plot ratio, plot area, gross floor area",
                        },
                    },
                },
                "required": ["attribute"],
            },
            {
                "name": "count_plots",
                "description": "Count the number of plots satisfying some constraints",
                "parameters": {
                    "type": "object",
                    "properties": {
                        "plot_constraints": {
                            "type": "string",
                            "description": "Conditions that define the plots of interest e.g. plots for commercial usage with gross floor area less than 1000sqm",
                        }
                    },
                },
            },
            {
                "name": "compute_aggregate_plot_attribute",
                "description": "For a given subset of plots, compute statistics of a plot attribute e.g. smallest plot area, average gross floor area, maximum gross plot ratio",
                "parameters": {
                    "type": "object",
                    "properties": {
                        "plot_constraints": {
                            "type": "string",
                            "description": "Conditions that define the plots of interest e.g. plots for commercial usage with gross floor area less than 1000sqm",
                        },
                        "attribute_aggregate": {
                            "type": "string",
                            "description": "Aggregate of an attribute value e.g. average plot area, largest gross plot ratio",
                        },
                    },
                },
                "required": ["attribute_aggregate"],
            },
            {
                "name": "explain_concepts",
                "description": "Provide explainations for concepts related to land lots",
                "parameters": {
                    "type": "object",
                    "properties": {
                        "concepts": {
                            "type": "array",
                            "items": {
                                "type": "string",
                                "description": "Concept e.g. commercial plot, Business 1 land use, Residential/Institution",
                            },
                        }
                    },
                },
                "required": ["concepts"],
            },
        ]

    @cached_property
    def name2method(self):
        return {
            "lookup_plot_attribute": self.lookup_plot_attribute,
            "count_plots": self.count_plots,
            "compute_aggregate_plot_attribute": self.compute_aggregate_plot_attribute,
            "explain_concepts": self.explain_concepts,
        }

    def _parse_plot_constraints(self, plot_constraints: str):
        logger.info("Parsing plot constraints...")
        timestamp = time.time()
        constraints = self.plot_constraints_parser.parse(plot_constraints)
        latency = time.time() - timestamp
        logger.info("Parsed plot constraints: " + str(constraints))

        step = QAStep(
            action="parse_plot_constraints",
            arguments=plot_constraints,
            results=str(constraints),
            latency=latency,
        )

        return constraints, step

    def _align_attributes(self, attribute: str):
        logger.info("Aligning attribute keys...")
        timestamp = time.time()
        attr_key = self.plot_attr_key_aligner.align(
            "".join([x.capitalize() for x in attribute.split()])
        )
        latency = time.time() - timestamp
        logger.info("Aligned attribute keys: " + str(attr_key))

        step = QAStep(
            action="align_attribute_key",
            arguments=attribute,
            results=attr_key.value,
            latency=latency,
        )
        return attr_key, step

    def lookup_plot_attribute(self, plot_constraints: str, attribute: str):
        steps: List[QAStep] = []

        constraints, step = self._parse_plot_constraints(plot_constraints)
        steps.append(step)

        attr_key, step = self._align_attributes(attribute)
        steps.append(step)

        timestamp = time.time()
        data = self.agent.lookup_plot_attribute(
            plot_constraints=constraints, attr_key=attr_key
        )
        latency = time.time() - timestamp
        steps.append(
            QAStep(
                action="lookup_plot_attributes",
                arguments=dict(
                    plot_constraints=str(constraints),
                    attribute=attr_key.value,
                ),
                latency=latency,
            )
        )

        return "QA", steps, data

    def count_plots(self, plot_constraints: str):
        steps: List[QAStep] = []

        constraints, step = self._parse_plot_constraints(plot_constraints)
        steps.append(step)

        timestamp = time.time()
        data = self.agent.count_plots(constraints)
        latency = time.time() - timestamp
        steps.append(
            QAStep(action="count_plots", arguments=str(constraints), latency=latency)
        )

        return "QA", steps, data

    def compute_aggregate_plot_attribute(
        self, plot_constraints: str, attribute_aggregate: str
    ):
        steps: List[QAStep] = []

        constraints, step = self._parse_plot_constraints(plot_constraints)
        steps.append(step)

        logger.info("Parsing aggregate function...")
        timestamp = time.time()
        attr_agg = self.attr_agg_parser.parse(attribute_aggregate)
        latency = time.time() - timestamp
        logger.info("Parsed aggregate function: " + str(attr_agg))
        attr_agg_unpacked = tuple(x.value for x in attr_agg)
        steps.append(
            QAStep(
                action="parse_aggregate_functions",
                arguments=attribute_aggregate,
                results=attr_agg_unpacked,
                latency=latency,
            )
        )

        timestamp = time.time()
        data = self.agent.compute_aggregate_plot_attribute(
            plot_constraints=constraints, attr_agg=attr_agg
        )
        latency = time.time() - timestamp
        steps.append(
            QAStep(
                action="compute_aggregate_plot_attributes",
                arguments=dict(
                    plot_constraints=str(constraints), attr_aggs=attr_agg_unpacked
                ),
                latency=latency,
            )
        )

        return "QA", steps, data

    def explain_concepts(self, concepts: List[str]):
        steps: List[QAStep] = []

        timestamp = time.time()
        data = self.agent.retrieve_concepts(concepts)
        latency = time.time() - timestamp
        steps.append(
            QAStep(
                action="retrieve_concepts",
                arguments=concepts,
                latency=latency,
            )
        )

        return "IR", steps, data


def get_sg_land_lots_agent_connector(
    plot_attr_key_aligner: Annotated[
        EnumAligner[PlotAttrKey], Depends(get_plot_attr_key_aligner)
    ],
    plot_constraints_parser: Annotated[
        PlotConstraintsParser, Depends(get_plot_constraint_parser)
    ],
    agent: Annotated[SGLandLotsAgent, Depends(get_sg_land_lots_agent)],
    attr_agg_parser: Annotated[
        KeyAggregateParser[PlotAttrKey], Depends(get_plot_attr_agg_parser)
    ],
):
    return SGLandLotsAgentConnector(
        plot_attr_key_aligner=plot_attr_key_aligner,
        plot_constraints_parser=plot_constraints_parser,
        agent=agent,
        attr_agg_parser=attr_agg_parser,
    )
