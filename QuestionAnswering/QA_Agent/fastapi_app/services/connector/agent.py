from abc import abstractclassmethod, abstractmethod
from typing import Any, Callable, Dict, List, Tuple

from model.qa import QAStep


class IAgent:
    @abstractclassmethod
    def get_tools(cls) -> List[Dict[str, str]]:
        pass

    @abstractmethod
    def get_name2method(self) -> Dict[str, Callable[..., Tuple[List[QAStep], List[dict]]]]:
        pass

    def exec(self, method_name: str, args: dict):
        return self.get_name2method()[method_name](**args)
