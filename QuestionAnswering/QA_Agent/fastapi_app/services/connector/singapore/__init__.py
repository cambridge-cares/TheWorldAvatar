import logging
import time
from typing import Annotated, List

from fastapi import Depends

from model.qa import QAStep
from services.retrieve_docs import DocsRetriever, get_docs_retriever
from services.connector.agent_connector import IAgentConnector
from .constants import PlotAttrKey
from .agent import SingaporeLandLotsAgent, get_singapore_land_lots_agent
from .parse import (
    AttributeAggregateParser,
    PlotConstraintsParser,
    get_attribute_aggregate_parser,
    get_plot_constraint_parser,
)

logger = logging.getLogger(__name__)


class SingaporeLandLotsAgentConnector(IAgentConnector):
    _FUNCS = [
        {
            "name": "lookup_plot_attributes",
            "description": "Look up attributes of a subset of plots satisfying some constraints",
            "parameters": {
                "type": "object",
                "properties": {
                    "plot_constraints": {
                        "type": "string",
                        "description": "Conditions that define the plots of interest e.g. plots for commercial usage with gross floor area less than 1000sqm",
                    },
                    "attributes": {
                        "type": "array",
                        "items": {
                            "type": "string",
                            "description": "Attribute to query e.g. land use type, gross plot ratio, plot area, gross floor area",
                        },
                    },
                },
            },
            "required": ["attributes"],
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
            "name": "compute_aggregate_plot_attributes",
            "description": "For a given subset of plots, compute statistics of plot attributes e.g. smallest plot area, average gross floor area, maximum gross plot ratio",
            "parameters": {
                "type": "object",
                "properties": {
                    "plot_constraints": {
                        "type": "string",
                        "description": "Conditions that define the plots of interest e.g. plots for commercial usage with gross floor area less than 1000sqm",
                    },
                    "attribute_aggregates": {
                        "type": "array",
                        "items": {
                            "type": "string",
                            "description": "Aggregate of an attribute value e.g. average plot area, largest gross plot ratio",
                        },
                    },
                },
            },
            "required": ["attribute_aggregates"],
        },
    ]

    @classmethod
    def get_funcs(cls):
        return cls._FUNCS

    def __init__(
        self,
        plot_constraints_parser: PlotConstraintsParser,
        singapore_land_plots_agent: SingaporeLandLotsAgent,
        docs_retriever: DocsRetriever,
        attr_agg_parser: AttributeAggregateParser,
    ):
        self.plot_constraints_parser = plot_constraints_parser
        self.agent = singapore_land_plots_agent
        self.docs_retriever = docs_retriever
        self.attr_agg_parser = attr_agg_parser

    def get_name2method(self):
        return {
            "lookup_plot_attributes": self.lookup_plot_attributes,
            "count_plots": self.count_plots,
            "compute_aggregate_plot_attributes": self.compute_aggregate_plot_attributes,
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

    def _align_attributes(self, attributes: List[str]):
        logger.info("Aligning attribute keys...")
        timestamp = time.time()
        retrieved = self.docs_retriever.retrieve(
            key="singapore:attribute_keys",
            docs_getter=lambda: [x.value for x in PlotAttrKey],
            queries=["".join([x.capitalize() for x in attr]) for attr in attributes],
            k=1,
        )
        attr_keys = [PlotAttrKey(x[0][0]) for x in retrieved]
        latency = time.time() - timestamp
        logger.info("Aligned attribute keys: " + str(attr_keys))

        step = QAStep(
            action="align_attribute_keys",
            arguments=attributes,
            results=[x.value for x in attr_keys],
            latency=latency,
        )
        return attr_keys, step

    def lookup_plot_attributes(self, plot_constraints: str, attributes: List[str]):
        steps: List[QAStep] = []

        constraints, step = self._parse_plot_constraints(plot_constraints)
        steps.append(step)

        attr_keys, step = self._align_attributes(attributes)
        steps.append(step)

        timestamp = time.time()
        data = self.agent.lookup_plot_attributes(
            plot_constraints=constraints, attr_keys=attr_keys
        )
        latency = time.time() - timestamp
        steps.append(
            QAStep(
                action="lookup_plot_attributes",
                arguments=str(constraints),
                latency=latency,
            )
        )

        return steps, data

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

        return steps, data

    def compute_aggregate_plot_attributes(
        self, plot_constraints: str, attribute_aggregates: List[str]
    ):
        steps: List[QAStep] = []

        constraints, step = self._parse_plot_constraints(plot_constraints)
        steps.append(step)

        logger.info("Parsing aggregate functions...")
        timestamp = time.time()
        attr_aggs = [self.attr_agg_parser.parse(x) for x in attribute_aggregates]
        latency = time.time() - timestamp
        logger.info("Parsed aggregate functions: " + str(attr_aggs))
        attr_aggs_unpacked = [(x.value for x in y) for y in attr_aggs]
        steps.append(
            QAStep(
                action="parse_aggregate_functions",
                arguments=attribute_aggregates,
                results=attr_aggs_unpacked,
                latency=latency,
            )
        )

        timestamp = time.time()
        data = self.agent.compute_aggregate_plot_attributes(
            plot_constraints=constraints, attr_aggs=attr_aggs
        )
        latency = time.time() - timestamp
        steps.append(
            QAStep(
                action="compute_aggregate_plot_attributes",
                arguments=dict(
                    plot_constraints=str(constraints), attr_aggs=attr_aggs_unpacked
                ),
                latency=latency,
            )
        )

        return steps, data


def get_singapore_land_lots_agent_connector(
    plot_constraints_parser: Annotated[
        PlotConstraintsParser, Depends(get_plot_constraint_parser)
    ],
    singapore_land_lots_agent: Annotated[
        SingaporeLandLotsAgent, Depends(get_singapore_land_lots_agent)
    ],
    docs_retriever: Annotated[DocsRetriever, Depends(get_docs_retriever)],
    attr_agg_parser: Annotated[
        AttributeAggregateParser, Depends(get_attribute_aggregate_parser)
    ],
):
    return SingaporeLandLotsAgentConnector(
        plot_constraints_parser=plot_constraints_parser,
        singapore_land_plots_agent=singapore_land_lots_agent,
        docs_retriever=docs_retriever,
        attr_agg_parser=attr_agg_parser,
    )
