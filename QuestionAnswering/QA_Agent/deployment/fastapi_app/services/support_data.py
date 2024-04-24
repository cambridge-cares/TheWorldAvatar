from abc import ABC, abstractmethod
from typing import List, Tuple

from model.qa import QAData, QAStep


class DataSupporter(ABC):
    @abstractmethod
    def query(self, query: str) -> Tuple[List[QAStep], QAData]:
        pass
