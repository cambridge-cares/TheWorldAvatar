from abc import ABC
from typing import Callable, Dict, List, Tuple

from model.qa import QAData, QAStep


class Name2Func(ABC):
    def get_name2func(self) -> Dict[str, Callable[..., Tuple[List[QAStep], QAData]]]:
        pass
