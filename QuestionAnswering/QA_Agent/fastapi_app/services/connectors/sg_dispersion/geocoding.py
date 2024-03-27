from abc import ABC, abstractmethod
from functools import cache
from typing import Tuple

import requests


class IGeocoder(ABC):
    @abstractmethod
    def get_coords(self, location: str) -> Tuple[str, str]:
        pass


class NominatimGeocoder(IGeocoder):
    URL = "https://nominatim.openstreetmap.org/search?"

    def get_coords(self, location: str):
        query_params = {"q": location, "format": "json"}
        entries = requests.get(self.URL, params=query_params).json()
        entry = entries[0]
        return entry["lat"], entry["lon"]

@cache
def get_geocoder():
    return NominatimGeocoder()