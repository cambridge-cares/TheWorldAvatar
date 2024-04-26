import logging
import time
from typing import Annotated, List, Optional, Tuple

from fastapi import Depends

from model.qa import QAStep
from core.align_enum import BiEnumAligner
from core.parse import KeyAggregateParser
from services.connectors.agent_connector import AgentConnectorBase
from services.entity_store import EntityStore, get_entity_linker
from .model import PlotCatAttrKey, PlotNumAttrKey
from .agent import SGLandLotsAgent, get_sgLandLots_agent
from .parse import get_plotAttr_aggParser
from .align import get_plotAttrKey_aligner

logger = logging.getLogger(__name__)


class SGLandLotsAgentConnector(AgentConnectorBase):
    def __init__(
        self,
        agent: SGLandLotsAgent,
        attr_key_aligner: BiEnumAligner[PlotCatAttrKey, PlotNumAttrKey],
        entity_linker: EntityStore,
        attr_agg_parser: KeyAggregateParser[PlotNumAttrKey],
    ):
        self.agent = agent
        self.attr_key_aligner = attr_key_aligner
        self.entity_linker = entity_linker
        self.attr_agg_parser = attr_agg_parser

    @property
    def funcs(self):
        return [
            {
                "name": "explain_landUses",
                "description": "Provide definitions of given land use categories",
                "parameters": {
                    "type": "object",
                    "properties": {
                        "land_use_types": {
                            "type": "array",
                            "items": {"type": "string"},
                            "description": "Array of land use classifications e.g. commercial, education, business 1",
                        }
                    },
                },
                "required": ["land_use_types"],
            },
            {
                "name": "count_plots",
                "description": "Count the number of plots satisfying some constraints",
                "parameters": {
                    "type": "object",
                    "properties": {
                        "land_use_type": {
                            "type": "string",
                            "description": "Land use classification e.g. commerical, education",
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

    @property
    def name2method(self):
        return {
            "explain_landUses": self.explain_landUses,
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
        candidates = self.entity_linker.link(land_use_type, "LandUseType")
        latency = time.time() - timestamp

        labels = [self.entity_linker.lookup_label(iri) for iri in candidates]
        step = QAStep(
            action="align_land_use_type",
            arguments=land_use_type,
            results=labels,
            latency=latency,
        )

        data = list(zip(candidates, labels))
        logger.info("Aligned land use classification: " + str(data))

        return data, step

    def explain_landUses(self, land_use_types: List[str]):
        steps: List[QAStep] = []

        aligned_land_uses: List[Tuple[str, Optional[str]]] = []
        for land_use in land_use_types:
            aligned, step = self._align_land_use_type(land_use)
            aligned_land_uses.extend(aligned)
            steps.append(step)

        timestamp = time.time()
        data = self.agent.explain_landUses(
            land_use_types=[iri for iri, _ in aligned_land_uses]
        )
        latency = time.time() - timestamp
        steps.append(
            QAStep(
                action="explain_landUses",
                arguments=dict(
                    land_use_types=[label for _, label in aligned_land_uses]
                ),
                latency=latency,
            )
        )

        return steps, data

    def count_plots(self, land_use_type: Optional[str] = None):
        steps: List[QAStep] = []

        if land_use_type:
            aligned_land_uses, step = self._align_land_use_type(land_use_type)
            steps.append(step)
        else:
            land_use_type = None

        timestamp = time.time()
        data = self.agent.count_plots(
            land_use_types=[iri for iri, _ in aligned_land_uses]
        )
        latency = time.time() - timestamp
        steps.append(
            QAStep(
                action="count_plots",
                arguments=dict(land_use_type=[label for _, label in aligned_land_uses]),
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
            aligned_land_uses, step = self._align_land_use_type(land_use_type)
            steps.append(step)
        else:
            aligned_land_uses = None

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
            land_use_types=[iri for iri, _ in aligned_land_uses],
        )
        latency = time.time() - timestamp
        steps.append(
            QAStep(
                action="compute_aggregate_plot_attributes",
                arguments=dict(
                    land_use_type=[label for _, label in aligned_land_uses],
                    attr_aggs=attr_agg_unpacked,
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
    entity_linker: Annotated[EntityStore, Depends(get_entity_linker)],
    attr_agg_parser: Annotated[
        KeyAggregateParser[PlotNumAttrKey], Depends(get_plotAttr_aggParser)
    ],
):
    return SGLandLotsAgentConnector(
        agent=agent,
        attr_key_aligner=attr_key_aligner,
        entity_linker=entity_linker,
        attr_agg_parser=attr_agg_parser,
    )
