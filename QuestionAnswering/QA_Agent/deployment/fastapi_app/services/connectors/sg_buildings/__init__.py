from functools import cache
import logging
import time
from typing import Annotated, List, Optional

from fastapi import Depends

from model.qa import QAStep
from services.core.align_enum import EnumAligner
from services.connectors.sg_buildings.model import PropertyUsage
from services.connectors.agent_connector import AgentConnectorBase
from .agent import SGBuildingsAgent, get_sgBuildings_agent
from .align import get_propertyUsage_aligner

logger = logging.getLogger(__name__)


class SGBuildingsAgentConnector(AgentConnectorBase):
    def __init__(
        self,
        agent: SGBuildingsAgent,
        property_usage_aligner: EnumAligner[PropertyUsage],
    ):
        self.agent = agent
        self.property_usage_aligner = property_usage_aligner

    @property
    def funcs(self):
        return [
            {
                "name": "count_buildings",
                "description": "Count number of buildings that fulfil some criteria",
                "parameters": {
                    "type": "object",
                    "properties": {
                        "usage": {
                            "type": "string",
                            "description": "Building usage e.g. domestic, education",
                        },
                        "groupby_usage": {
                            "type": "boolean",
                            "description": "Whether to group results by usage",
                        },
                    },
                },
            }
        ]

    @property
    def name2method(
        self,
    ):
        return {"count_buildings": self.count_buildings}

    def count_buildings(self, usage: Optional[str] = None, groupby_usage: bool = False):
        steps: List[QAStep] = []

        if usage:
            logger.info("Aligning property usage: " + str(usage))
            timestamp = time.time()
            aligned_usage = self.property_usage_aligner.align(usage)
            latency = time.time() - timestamp
            logger.info("Aligned property usage: " + str(aligned_usage))
            steps.append(
                QAStep(
                    action="align_property_usage",
                    arguments=usage,
                    results=aligned_usage.value,
                    latency=latency,
                )
            )
        else:
            aligned_usage = None

        timestamp = time.time()
        data = self.agent.count_buildings(aligned_usage, groupby_usage)
        latency = time.time() - timestamp
        steps.append(
            QAStep(
                action="count_propertyUsage",
                arguments=dict(
                    usage=aligned_usage,
                    groupby_usage=groupby_usage,
                ),
                latency=latency,
            )
        )

        return steps, data


@cache
def get_sgBuildlings_agentConnector(
    sgBuildings_agent: Annotated[SGBuildingsAgent, Depends(get_sgBuildings_agent)],
    propertyUsage_aligner: Annotated[
        EnumAligner[PropertyUsage], Depends(get_propertyUsage_aligner)
    ],
):
    return SGBuildingsAgentConnector(
        agent=sgBuildings_agent, property_usage_aligner=propertyUsage_aligner
    )
