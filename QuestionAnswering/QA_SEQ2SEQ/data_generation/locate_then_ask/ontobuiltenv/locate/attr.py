from abc import ABC, abstractmethod
from locate_then_ask.ontobuiltenv.model import OBEProperty

from locate_then_ask.query_graph import QueryGraph


class OBEAttrLocator(ABC):
    @abstractmethod
    def locate(
        self, query_graph: QueryGraph, entity: OBEProperty
    ) -> str:
        pass
