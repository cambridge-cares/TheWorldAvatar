from functools import cache
from typing import Any, Dict, List

from pydantic import TypeAdapter
import requests

from services.geocoding.base import IGeocoder, Place


class NominatimGeocoder(IGeocoder):
    URL = "https://nominatim.openstreetmap.org/search?"

    def __init__(self):
        self.geocode_res_adapter = TypeAdapter(List[Dict[str, Any]])

    def search(self, location: str):
        query_params = {"q": location, "format": "json"}
        res = requests.get(self.URL, params=query_params)
        res.raise_for_status()

        entries = self.geocode_res_adapter.validate_json(res.text)
        if not entries:
            return None

        entry = entries[0]

        return Place(
            lat=entry["lat"],
            lon=entry["lon"],
            name=entry.get("name", entry["display_name"]),
        )


@cache
def get_nominatim_geocoder():
    return NominatimGeocoder()
