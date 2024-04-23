from abc import ABC, abstractmethod
from typing import List

IRI = str

class IEntityLinker(ABC):
    @abstractmethod
    def link(self, surface_form: str) -> List[IRI]:
        pass