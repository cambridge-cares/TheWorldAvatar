from functools import cached_property
import logging
import time
from typing import Annotated, List, Optional

from fastapi import Depends

from model.qa import QAStep
from services.core.align_enum import BiEnumAligner
from services.core.retrieve_docs import DocsRetriever
from services.core.parse import (
    KeyAggregateParser,
    NumericalConstraintParser,
    get_numConstraint_parser,
)
from services.connectors.agent_connector import AgentConnectorBase
from .model import PlotCatAttrKey, PlotNumAttrKey
from .agent import SGLandLotsAgent, get_sgLandLots_agent
from .parse import get_plotAttr_aggParser
from .match import LandUseType, get_landUseType_retriever
from .align import get_plotAttrKey_aligner

logger = logging.getLogger(__name__)


class SGLandLotsAgentConnector(AgentConnectorBase):
    def __init__(
        self,
        agent: SGLandLotsAgent,
        attr_key_aligner: BiEnumAligner[PlotCatAttrKey, PlotNumAttrKey],
        land_use_type_retriever: DocsRetriever[LandUseType],
        num_constraint_parser: NumericalConstraintParser,
        attr_agg_parser: KeyAggregateParser[PlotNumAttrKey],
    ):
        self.agent = agent
        self.attr_key_aligner = attr_key_aligner
        self.land_use_type_retriever = land_use_type_retriever
        self.num_constraint_parser = num_constraint_parser
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
                        "attribute": {
                            "type": "string",
                            "description": "Attribute to query e.g. land use type, gross plot ratio, plot area, gross floor area",
                        },
                        "land_use_type": {
                            "type": "string",
                            "description": "Land use classification e.g. commerical, education",
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
                        "land_use_type": {
                            "type": "string",
                            "description": "Land use classification etc. commerical, education",
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
                        "attribute_aggregate": {
                            "type": "string",
                            "description": "Aggregate of an attribute value e.g. average plot area, largest gross plot ratio",
                        },
                        "land_use_type": {
                            "type": "string",
                            "description": "Land use classification etc. commerical, education",
                        },
                    },
                },
                "required": ["attribute_aggregate"],
            },
        ]

    @cached_property
    def name2method(self):
        return {
            "lookup_plot_attribute": self.lookup_plot_attribute,
            "count_plots": self.count_plots,
            "compute_aggregate_plot_attribute": self.compute_aggregate_plot_attribute,
        }

    def _align_attributes(self, attribute: str):
        logger.info("Aligning attribute keys...")
        timestamp = time.time()
        attr_key = self.attr_key_aligner.align(
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

    def _align_land_use_type(self, land_use_type: str):
        logger.info("Align land use classification: " + land_use_type)
        timestamp = time.time()
        land_use_type_iri = self.land_use_type_retriever.match(land_use_type, key="IRI")
        latency = time.time() - timestamp
        step = QAStep(
            action="align_land_use_type",
            arguments=land_use_type,
            results=land_use_type_iri,
            latency=latency,
        )

        return land_use_type_iri, step

    def lookup_plot_attribute(
        self, attribute: str, land_use_type: Optional[str] = None
    ):
        steps: List[QAStep] = []

        attr_key, step = self._align_attributes(attribute)
        steps.append(step)

        if land_use_type:
            land_use_type_iri, step = self._align_land_use_type(land_use_type)
            steps.append(step)
        else:
            land_use_type_iri = None

        timestamp = time.time()
        data = self.agent.lookup_plot_attribute(
            attr_key=attr_key,
            land_use_type_iri=land_use_type_iri,
        )
        latency = time.time() - timestamp
        steps.append(
            QAStep(
                action="lookup_plot_attributes",
                arguments=dict(
                    land_use_type=land_use_type,
                    attribute=attr_key.value,
                ),
                latency=latency,
            )
        )

        return steps, data

    def count_plots(self, land_use_type: Optional[str] = None):
        steps: List[QAStep] = []

        if land_use_type:
            land_use_type_iri, step = self._align_land_use_type(land_use_type)
            steps.append(step)
        else:
            land_use_type_iri = None

        timestamp = time.time()
        data = self.agent.count_plots(land_use_type_iri=land_use_type_iri)
        latency = time.time() - timestamp
        steps.append(
            QAStep(
                action="count_plots",
                arguments=dict(land_use_type_iri=land_use_type_iri),
                latency=latency,
            )
        )

        return steps, data

    def compute_aggregate_plot_attribute(
        self,
        attribute_aggregate: str,
        land_use_type: Optional[str] = None,
    ):
        steps: List[QAStep] = []

        if land_use_type:
            land_use_type_iri, step = self._align_land_use_type(land_use_type)
            steps.append(step)
        else:
            land_use_type_iri = None

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
            attr_agg=attr_agg,
            land_use_type_iri=land_use_type_iri,
        )
        latency = time.time() - timestamp
        steps.append(
            QAStep(
                action="compute_aggregate_plot_attributes",
                arguments=dict(
                    land_use_type_iri=land_use_type_iri, attr_aggs=attr_agg_unpacked
                ),
                latency=latency,
            )
        )

        return steps, data


def get_sgLandLots_agentConnector(
    agent: Annotated[SGLandLotsAgent, Depends(get_sgLandLots_agent)],
    attr_key_aligner: Annotated[
        BiEnumAligner[PlotCatAttrKey, PlotNumAttrKey], Depends(get_plotAttrKey_aligner)
    ],
    land_use_type_retriever: Annotated[
        DocsRetriever[LandUseType], Depends(get_landUseType_retriever)
    ],
    num_constraint_parser: Annotated[
        NumericalConstraintParser, Depends(get_numConstraint_parser)
    ],
    attr_agg_parser: Annotated[
        KeyAggregateParser[PlotNumAttrKey], Depends(get_plotAttr_aggParser)
    ],
):
    return SGLandLotsAgentConnector(
        agent=agent,
        attr_key_aligner=attr_key_aligner,
        land_use_type_retriever=land_use_type_retriever,
        num_constraint_parser=num_constraint_parser,
        attr_agg_parser=attr_agg_parser,
    )
