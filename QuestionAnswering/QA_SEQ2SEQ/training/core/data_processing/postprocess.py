from abc import ABC, abstractmethod

from core.sparql import SparqlQuery


class PostProcessor(ABC):
    @abstractmethod
    def postprocess(self, query: SparqlQuery, **kwargs) -> SparqlQuery:
        pass