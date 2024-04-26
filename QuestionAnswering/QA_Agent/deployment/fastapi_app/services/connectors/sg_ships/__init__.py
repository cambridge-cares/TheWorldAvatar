from functools import cache
import time
from typing import Annotated, List, Optional

from fastapi import Depends

from model.qa import QAStep
from services.connectors.agent_connector import AgentConnectorBase
from .agent import SGShipsAgent, get_sgShips_agent


class SGShipsAgentConnector(AgentConnectorBase):
    def __init__(self, agent: SGShipsAgent):
        self.agent = agent

    @property
    def funcs(self):
        return [
            {
                "name": "lookup_ship_attributes",
                "description": "Given ship name or Maritime Mobile Service Identity (MMSI), look up ship attributes, including maximum static draught, dimension, IMO number, ship type, and call sign",
                "parameters": {
                    "type": "object",
                    "properties": {
                        "name": {"type": "string", "description": "Ship name"},
                        "mmsi": {
                            "type": "string",
                            "description": "Maritime Mobile Service Identity (MMSI) represented by a nine-digit number",
                        },
                    },
                },
            },
            {
                "name": "lookup_ship_timeseries",
                "description": "Given ship name or Maritime Mobile Service Identity (MMSI), look up speed over ground, course over ground, latitude, longitude",
                "parameters": {
                    "type": "object",
                    "properties": {
                        "name": {"type": "string", "description": "Ship name"},
                        "mmsi": {
                            "type": "string",
                            "description": "Maritime Mobile Service Identity (MMSI) represented by a nine-digit number",
                        },
                    },
                },
            },
        ]

    @property
    def name2method(self):
        return {
            "lookup_ship_attributes": self.lookup_ship_attributes,
            "lookup_ship_timeseries": self.lookup_ship_timeseries,
        }

    def lookup_ship_attributes(
        self, name: Optional[str] = None, mmsi: Optional[str] = None
    ):
        steps: List[QAStep] = []

        timestamp = time.time()
        data = self.agent.lookup_ship_attributes(name, mmsi)
        latency = time.time() - timestamp
        steps.append(
            QAStep(
                action="lookup_ship_attributes",
                arguments=dict(name=name, mmsi=mmsi),
                latency=latency,
            )
        )

        return steps, data

    def lookup_ship_timeseries(
        self, name: Optional[str] = None, mmsi: Optional[str] = None
    ):
        steps: List[QAStep] = []

        timestamp = time.time()
        data = self.agent.lookup_ship_timeseries(name, mmsi)
        latency = time.time() - timestamp
        steps.append(
            QAStep(
                action="lookup_ship_timeseries",
                arguments=dict(name=name, mmsi=mmsi),
                latency=latency,
            )
        )

        return steps, data


@cache
def get_sgShips_agentConnector(
    agent: Annotated[SGShipsAgent, Depends(get_sgShips_agent)]
):
    return SGShipsAgentConnector(agent)
