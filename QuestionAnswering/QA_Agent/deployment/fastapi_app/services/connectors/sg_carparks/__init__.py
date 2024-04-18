from functools import cache, cached_property
import logging
import time
from typing import Annotated, List

from fastapi import Depends

from model.qa import QAStep
from services.connectors.sg_dispersion.geocoding import IGeocoder, get_geocoder
from services.connectors.sg_carparks.agent import SGCarparksAgent, get_sgCarpark_agent
from services.connectors.agent_connector import AgentConnectorBase


logger = logging.getLogger(__name__)


class SGCarparksAgentConnector(AgentConnectorBase):
    def __init__(self, agent: SGCarparksAgent, geocoder: IGeocoder):
        self.agent = agent
        self.geocoder = geocoder

    @cached_property
    def funcs(self):
        return [
            {
                "name": "find_nearest_carpark",
                "description": "Given a location, find the k nearest carparks and their lot availabilities",
                "parameters": {
                    "type": "object",
                    "properties": {
                        "location": {
                            "type": "string",
                            "description": "Location to find nearest carpark",
                        },
                        "limit": {
                            "type": "number",
                            "description": "Number of carparks to return",
                        },
                    },
                    "required": ["location"],
                },
            }
        ]

    @cached_property
    def name2method(self):
        return {"find_nearest_carpark": self.find_nearest_carpark}

    def find_nearest_carpark(self, location: str, limit: int = 1):
        steps: List[QAStep] = []

        logger.info("Get coordinates for the location: " + location)
        timestamp = time.time()
        place = self.geocoder.search(location)
        latency = time.time() - timestamp
        logger.info("Geo-decoded data: " + str(place))
        steps.append(
            QAStep(
                action="geodecode",
                arguments=location,
                results=str(place),
                latency=latency,
            )
        )

        timestamp = time.time()
        data = self.agent.find_nearest_carpark(
            lat=place.lat, lon=place.lon, limit=limit
        )
        latency = time.time() - timestamp
        steps.append(
            QAStep(
                action="get_nearest_carpark",
                arguments=str(place),
                latency=latency,
            )
        )

        return steps, data


@cache
def get_sgCarparks_agentConnector(
    agent: Annotated[SGCarparksAgent, Depends(get_sgCarpark_agent)],
    geodecoder: Annotated[IGeocoder, Depends(get_geocoder)],
):
    return SGCarparksAgentConnector(agent=agent, geocoder=geodecoder)
