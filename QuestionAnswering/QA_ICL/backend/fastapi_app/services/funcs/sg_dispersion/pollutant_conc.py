from functools import cache
from typing import Annotated, List
from fastapi import Depends
from pydantic import BaseModel, Field

from config import AppSettings, get_app_settings
from services.requests import request_get_obj


class PollutantConcResponse(BaseModel):
    time: List[str]
    uHC: List[float]
    NOx: List[float]
    SO2: List[float]
    PM25: List[float] = Field(..., alias="PM2.5")
    PM10: List[float]
    CO: List[float]


class PollutantConcClient:
    def __init__(self, url: str):
        self.url = url

    def get(self, lat: str, lon: str):
        return request_get_obj(
            self.url,
            params={"lat": lat, "lon": lon},
            response_type=PollutantConcResponse,
        )


@cache
def get_pollutantConc_client(
    settings: Annotated[AppSettings, Depends(get_app_settings)]
):
    return PollutantConcClient(settings.singapore_endpoints.pollutant_concentration)
