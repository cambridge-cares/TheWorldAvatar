from functools import cache
import os
from typing import List, Optional, Tuple

from pydantic import BaseModel, Field, TypeAdapter
import requests

from services.geocoding.base import Place, IGeocoder


# https://docs.locationiq.com/reference/search
class LocationIQGecodingResponse(BaseModel):
    place_id: str
    licence: str
    osm_type: Optional[str] = None
    osm_id: Optional[str] = None
    lat: str
    lon: str
    display_name: str
    cls: Optional[str] = Field(default=None, alias="class")
    type: Optional[str] = None
    importance: Optional[float] = None
    address: Optional[dict] = None
    boundingbox: Tuple[str, str, str, str]
    namedetails: Optional[dict] = None
    extratags: Optional[dict] = None
    geojson: Optional[dict] = None
    geokml: Optional[str] = None
    svg: Optional[str] = None
    geotext: Optional[str] = None
    icon: Optional[str] = None
    matchquality: Optional[dict] = None
    postaladdress: Optional[str] = None


class LocationIQGeocoder(IGeocoder):
    URL = "https://us1.locationiq.com/v1/search"

    def __init__(self, api_key: str):
        self.api_key = api_key
        self.geocode_res_adapter = TypeAdapter(List[LocationIQGecodingResponse])

    def search(self, location: str):
        query_params = {"key": self.api_key, "q": location, "format": "json"}
        res = requests.get(self.URL, params=query_params)
        res.raise_for_status()

        entries = self.geocode_res_adapter.validate_json(res.text)
        if not entries:
            return None

        entry = entries[0]

        return Place(lat=entry.lat, lon=entry.lon, name=entry.display_name)


@cache
def get_locationIq_geocoder():
    return LocationIQGeocoder(api_key=os.getenv("LOCATION_IQ_API_KEY"))
