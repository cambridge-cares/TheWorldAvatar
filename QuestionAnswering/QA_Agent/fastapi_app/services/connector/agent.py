from abc import abstractclassmethod, abstractmethod
from typing import Any, Callable, Dict, List


class IAgent:
    @abstractclassmethod
    def get_tools(cls) -> List[Dict[str, str]]:
        pass

    @abstractmethod
    def get_name2method(self) -> Dict[str, Callable[..., List[Dict[str, Any]]]]:
        pass

    def exec(self, method_name: str, args: dict):
        return self.get_name2method()[method_name](**args)
