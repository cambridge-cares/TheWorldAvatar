from abc import ABC, abstractmethod
from functools import cache

from pydantic.dataclasses import dataclass
import requests


@dataclass
class Place:
    lat: str
    lon: str
    name: str
    display_name: str


class IGeocoder(ABC):
    @abstractmethod
    def search(self, location: str) -> Place:
        pass


class NominatimGeocoder(IGeocoder):
    URL = "https://nominatim.openstreetmap.org/search?"

    def search(self, location: str):
        query_params = {"q": location, "format": "json"}
        entries = requests.get(self.URL, params=query_params).json()
        # TODO: handle when entries is empty
        entry = entries[0]
        return Place(
            lat=entry["lat"],
            lon=entry["lon"],
            name=entry["name"],
            display_name=entry["display_name"],
        )


@cache
def get_geocoder():
    return NominatimGeocoder()
