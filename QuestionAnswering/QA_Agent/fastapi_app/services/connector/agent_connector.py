from abc import abstractclassmethod, abstractmethod
from typing import Callable, Dict, List, Tuple

from model.qa import QAData, QAStep


class IAgentConnector:
    @abstractclassmethod
    def get_funcs(cls) -> List[Dict[str, str]]:
        pass

    @abstractmethod
    def get_name2method(self) -> Dict[str, Callable[..., Tuple[List[QAStep], QAData]]]:
        pass

    def exec(self, method_name: str, args: dict):
        return self.get_name2method()[method_name](**args)
