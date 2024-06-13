from abc import ABC, abstractmethod
from typing import Protocol, Sequence

from model.rdf_orm import RDFEntity


class RDFEntityGetter(Protocol):
    def __call__(self, iris: Sequence[str]) -> list[RDFEntity | None]: ...


class Cls2GetterRDFStore(ABC):
    @property
    @abstractmethod
    def cls2getter(self) -> dict[str, RDFEntityGetter]: ...
