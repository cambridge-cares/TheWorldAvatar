from functools import cache
import os

from pydantic import BaseModel, Field, TypeAdapter
import requests

from services.geocoding.base import Place, IGeocoder


# https://docs.locationiq.com/reference/search
class LocationIQGecodingResponse(BaseModel):
    place_id: str
    licence: str
    osm_type: str | None = None
    osm_id: str | None = None
    lat: str
    lon: str
    display_name: str
    cls: str | None = Field(default=None, alias="class")
    type: str | None = None
    importance: float | None = None
    address: dict | None = None
    boundingbox: tuple[str, str, str, str]
    namedetails: dict | None = None
    extratags: dict | None = None
    geojson: dict | None = None
    geokml: str | None = None
    svg: str | None = None
    geotext: str | None = None
    icon: str | None = None
    matchquality: dict | None = None
    postaladdress: str | None = None


class LocationIQGeocoder(IGeocoder):
    URL = "https://us1.locationiq.com/v1/search"

    def __init__(self, api_key: str):
        self.api_key = api_key
        self.geocode_res_adapter = TypeAdapter(list[LocationIQGecodingResponse])

    def search(self, location: str):
        query_params = {"key": self.api_key, "q": location, "format": "json"}
        res = requests.get(self.URL, params=query_params)

        try:
            res.raise_for_status()
        except Exception as e:
            e.args = e.args + (res.raw,)
            raise e
        
        entries = self.geocode_res_adapter.validate_json(res.text)
        if not entries:
            return None

        entry = entries[0]

        return Place(lat=entry.lat, lon=entry.lon, name=entry.display_name)


@cache
def get_locationIq_geocoder():
    return LocationIQGeocoder(api_key=os.getenv("LOCATION_IQ_API_KEY"))
