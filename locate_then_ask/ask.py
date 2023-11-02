from abc import ABC, abstractmethod
from dataclasses import dataclass

from locate_then_ask.query_graph import QueryGraph


@dataclass
class AskDatum:
    query_graph: QueryGraph
    query_sparql: str
    verbalization: str


class Asker(ABC):
    @abstractmethod
    def ask_name(self, query_graph: QueryGraph, verbalization: str) -> AskDatum:
        pass

    @abstractmethod
    def ask_count(self, query_graph: QueryGraph, verbalization: str) -> AskDatum:
        pass

    @abstractmethod
    def ask_attribute(
        self, query_graph: QueryGraph, verbalization: str, attr_num: int = 1
    ) -> AskDatum:
        pass
