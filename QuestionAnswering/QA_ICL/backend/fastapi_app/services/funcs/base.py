from abc import ABC, abstractmethod
from typing import Callable

from model.structured_answer import DataItem


class Name2Func(ABC):
    @abstractmethod
    def get_name2func(
        self,
    ) -> dict[str, Callable[..., list[DataItem]]]: ...
