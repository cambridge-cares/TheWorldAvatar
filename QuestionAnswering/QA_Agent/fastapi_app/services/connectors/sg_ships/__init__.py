from functools import cache, cached_property
from typing import Annotated, Optional

from fastapi import Depends

from services.connectors.agent_connector import AgentConnectorBase
from .agent import SGShipsAgent, get_sgShips_agent


class SGShipsAgentConnector(AgentConnectorBase):
    def __init__(self, agent: SGShipsAgent):
        self.agent = agent

    @cached_property
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

    @cached_property
    def name2method(self):
        return {
            "lookup_ship_attributes": self.lookup_ship_attributes,
            "lookup_ship_timeseries": self.lookup_ship_timeseries,
        }

    def lookup_ship_attributes(
        self, name: Optional[str] = None, mmsi: Optional[str] = None
    ):
        return [], self.agent.lookup_ship_attributes(name, mmsi)

    def lookup_ship_timeseries(
        self, name: Optional[str] = None, mmsi: Optional[str] = None
    ):
        return [], self.agent.lookup_ship_timeseries(name, mmsi)


@cache
def get_sgShips_agentConnector(
    agent: Annotated[SGShipsAgent, Depends(get_sgShips_agent)]
):
    return SGShipsAgentConnector(agent)
