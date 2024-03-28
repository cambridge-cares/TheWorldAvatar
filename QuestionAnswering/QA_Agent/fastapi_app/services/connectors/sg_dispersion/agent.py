from functools import cache
import os
from typing import Annotated, Tuple

from fastapi import Depends
import requests

from model.qa import QAData


class SGDispersionAgent:
    def __init__(self, url: str):
        self.url = url

    def get_pollutant_concentrations(self, lat: str, lon: str):
        query_params = {"lat": lat, "lon": lon}
        res = requests.get(self.url, params=query_params)
        res.raise_for_status()
        res = res.json()

        timestamps = res["time"]
        return QAData(
            vars=["pollutant", "timeseries"],
            bindings=[
                {
                    "pollutant": key,
                    "timeseries": list(zip(timestamps, value)),
                }
                for key, value in res.items()
                if key != "time"
            ],
        )


@cache
def get_dispersion_url():
    return os.getenv("ENDPOINT_GET_POLLUTANT_CONCENTRATIONS")


@cache
def get_sgDisperson_agent(dispersion_url: Annotated[str, Depends(get_dispersion_url)]):
    return SGDispersionAgent(url=dispersion_url)
