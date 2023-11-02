from abc import ABC, abstractmethod
from typing import Iterable, Optional, Tuple

from locate_then_ask.query_graph import QueryGraph


class EntityLocator(ABC):
    @abstractmethod
    def locate_entity_name(self, entity_iris: Iterable[str]) -> Tuple[QueryGraph, str]:
        pass

    @abstractmethod
    def locate_concept_name(self, entity_iri: str) -> Tuple[QueryGraph, str]:
        pass

    @abstractmethod
    def locate_concept_and_literal(self, entity_iri: str, query_graph: Optional[QueryGraph]) -> Tuple[QueryGraph, str]:
        pass
    
    @abstractmethod
    def locate_intersection(self, entity_iri: str, cond_num: int) -> Tuple[QueryGraph, str]:
        pass