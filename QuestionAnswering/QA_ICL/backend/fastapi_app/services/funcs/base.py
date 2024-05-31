from abc import ABC
from typing import Callable, Dict, List

from services.model import DataItem


class Name2Func(ABC):
    def get_name2func(
        self,
    ) -> Dict[str, Callable[..., List[DataItem]]]:
        pass
