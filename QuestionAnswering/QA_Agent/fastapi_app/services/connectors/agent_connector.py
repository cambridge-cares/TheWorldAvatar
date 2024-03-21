from abc import abstractmethod
from typing import Callable, Dict, List, Tuple

from model.qa import QAData, QAStep


class AgentConnectorBase:
    @property
    @abstractmethod
    def funcs(self) -> List[Dict[str, str]]:
        pass

    @property
    @abstractmethod
    def name2method(
        self,
    ) -> Dict[str, Callable[..., Tuple[List[QAStep], QAData]]]:
        pass

    def exec(self, method_name: str, args: dict):
        return self.name2method[method_name](**args)
