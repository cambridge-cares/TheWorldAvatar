from abc import ABC, abstractmethod

from pydantic import BaseModel


class Place(BaseModel):
    lat: str
    lon: str
    name: str


class IGeocoder(ABC):
    @abstractmethod
    def search(self, location: str) -> Place | None: ...
