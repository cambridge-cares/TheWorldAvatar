from abc import ABC
from typing import Callable

from model.qa import DataItem


class Name2Func(ABC):
    def get_name2func(
        self,
    ) -> dict[str, Callable[..., list[DataItem]]]:
        pass
