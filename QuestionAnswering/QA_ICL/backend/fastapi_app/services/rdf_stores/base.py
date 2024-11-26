from abc import ABC, abstractmethod
from typing import Protocol

from model.rdf_ogm import RDFEntity
from services.sparql import SparqlClient


class RDFEntityGetter(Protocol):
    def __call__(
        self,
        iris: list[str] | tuple[str],
        sparql_client: str | SparqlClient | None = None,
    ) -> list[RDFEntity | None]: ...


class Cls2NodeGetter(ABC):
    @property
    @abstractmethod
    def cls2getter(self) -> dict[str, RDFEntityGetter]: ...
