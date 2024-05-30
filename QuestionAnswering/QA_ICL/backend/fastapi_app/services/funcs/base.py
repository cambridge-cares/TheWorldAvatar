from abc import ABC
from typing import Callable, Dict, List, Tuple

from controllers.qa.model import QAStep
from services.model import DataItem


class Name2Func(ABC):
    def get_name2func(
        self,
    ) -> Dict[str, Callable[..., Tuple[List[QAStep], List[DataItem]]]]:
        pass
