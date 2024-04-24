from abc import ABC, abstractmethod
from typing import List, Tuple

EntityIRI = str
EntityLabel = str


class IEntityLinker(ABC):
    @abstractmethod
    def link(self, surface_form: str, k: int) -> List[Tuple[EntityIRI, EntityLabel]]:
        pass
