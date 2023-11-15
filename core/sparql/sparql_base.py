from abc import ABC, abstractmethod
from typing import Hashable, Tuple


class SparqlBase(ABC):
    def tolines(self):
        return [str(self)]

    @abstractmethod
    def _keys(self) -> Tuple[Hashable, ...]:
        pass

    def __eq__(self, other):
        if not isinstance(other, SparqlBase):
            return False
        return self._keys() == other._keys()