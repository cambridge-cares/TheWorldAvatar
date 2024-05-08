from abc import ABC, abstractmethod
from typing import Optional

from pydantic import BaseModel


class Place(BaseModel):
    lat: str
    lon: str
    name: str


class IGeocoder(ABC):
    @abstractmethod
    def search(self, location: str) -> Optional[Place]:
        pass
