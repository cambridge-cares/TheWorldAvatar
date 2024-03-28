from functools import cached_property
import logging
import time
from typing import Annotated, List

from fastapi import Depends

from model.qa import QAStep
from services.connectors.agent_connector import AgentConnectorBase
from .agent import SGDispersionAgent, get_sgDisperson_agent
from .geocoding import IGeocoder, get_geocoder

logger = logging.getLogger(__name__)


class SGDispersionAgentConnector(AgentConnectorBase):
    def __init__(self, agent: SGDispersionAgent, geocoder: IGeocoder):
        self.agent = agent
        self.geocoder = geocoder

    @cached_property
    def funcs(self):
        return [
            {
                "name": "get_pollutant_concentrations",
                "description": "Obtain pollutant readings at a given location",
                "parameters": {
                    "type": "object",
                    "properties": {
                        "location": {
                            "type": "string",
                            "description": "Location to get pollutant readings",
                        }
                    },
                },
            }
        ]

    @cached_property
    def name2method(self):
        return {"get_pollutant_concentrations": self.get_pollutant_concentrations}

    def get_pollutant_concentrations(self, location: str):
        steps: List[QAStep] = []

        logger.info("Get coordinates for the location: " + location)
        timestamp = time.time()
        place = self.geocoder.search(location)
        latency = time.time() - timestamp
        logger.info("Geo-decoded data: " + str(place))
        steps.append(
            QAStep(
                action="geodecode", arguments=location, results=str(place), latency=latency
            )
        )

        timestamp = time.time()
        data = self.agent.get_pollutant_concentrations(lat=place.lat, lon=place.lon)
        data.title = "Pollutant concentrations (µg/m³) in " + place.display_name
        latency = time.time() - timestamp
        steps.append(
            QAStep(
                action="get_pollutant_concentrations", arguments=str(place), latency=latency
            )
        )

        return steps, data


def get_sgDispersion_agentConnector(
    agent: Annotated[SGDispersionAgent, Depends(get_sgDisperson_agent)],
    geocoder: Annotated[IGeocoder, Depends(get_geocoder)],
):
    return SGDispersionAgentConnector(agent=agent, geocoder=geocoder)
