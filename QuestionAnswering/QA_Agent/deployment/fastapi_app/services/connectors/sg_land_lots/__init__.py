from functools import cached_property
import logging
import time
from typing import Annotated, List, Optional

from fastapi import Depends

from model.qa import QAStep
from services.core.align_enum import BiEnumAligner
from services.core.retrieve_docs import DocsRetriever
from services.core.parse import KeyAggregateParser
from services.connectors.agent_connector import AgentConnectorBase
from .model import LandUseTypeNode, PlotCatAttrKey, PlotNumAttrKey
from .agent import SGLandLotsAgent, get_sgLandLots_agent
from .parse import get_plotAttr_aggParser
from .match import  get_landUseType_retriever
from .align import get_plotAttrKey_aligner

logger = logging.getLogger(__name__)


class SGLandLotsAgentConnector(AgentConnectorBase):
    def __init__(
        self,
        agent: SGLandLotsAgent,
        attr_key_aligner: BiEnumAligner[PlotCatAttrKey, PlotNumAttrKey],
        land_use_type_retriever: DocsRetriever[LandUseTypeNode],
        attr_agg_parser: KeyAggregateParser[PlotNumAttrKey],
    ):
        self.agent = agent
        self.attr_key_aligner = attr_key_aligner
        self.land_use_type_retriever = land_use_type_retriever
        self.attr_agg_parser = attr_agg_parser

    @cached_property
    def funcs(self):
        return [
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
                            "description": 'Aggregate of an attribute value e.g. "average plot area", "largest gross plot ratio"',
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

    def _align_land_use_types(self, land_use_type: str):
        logger.info("Align land use classification: " + land_use_type)

        timestamp = time.time()
        closest = self.land_use_type_retriever.retrieve(queries=[land_use_type], k=3)[0]
        aligned_land_use_types = [node["clsname"] for node, _ in closest]
        latency = time.time() - timestamp
        step = QAStep(
            action="align_land_use_type",
            arguments=land_use_type,
            results=aligned_land_use_types,
            latency=latency,
        )

        logger.info("Aligned land use classification: " + str(aligned_land_use_types))

        return aligned_land_use_types, step

    def count_plots(self, land_use_type: Optional[str] = None):
        steps: List[QAStep] = []

        if land_use_type:
            aligned_land_use_types, step = self._align_land_use_types(land_use_type)
            steps.append(step)
        else:
            land_use_type = None

        timestamp = time.time()
        data = self.agent.count_plots(land_use_types=aligned_land_use_types)
        latency = time.time() - timestamp
        steps.append(
            QAStep(
                action="count_plots",
                arguments=dict(land_use_type=aligned_land_use_types),
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
            aligned_land_use_types, step = self._align_land_use_types(land_use_type)
            steps.append(step)
        else:
            aligned_land_use_types = None

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
            land_use_types=aligned_land_use_types,
        )
        latency = time.time() - timestamp
        steps.append(
            QAStep(
                action="compute_aggregate_plot_attributes",
                arguments=dict(
                    land_use_type=aligned_land_use_types, attr_aggs=attr_agg_unpacked
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
        DocsRetriever[LandUseTypeNode], Depends(get_landUseType_retriever)
    ],
    attr_agg_parser: Annotated[
        KeyAggregateParser[PlotNumAttrKey], Depends(get_plotAttr_aggParser)
    ],
):
    return SGLandLotsAgentConnector(
        agent=agent,
        attr_key_aligner=attr_key_aligner,
        land_use_type_retriever=land_use_type_retriever,
        attr_agg_parser=attr_agg_parser,
    )
