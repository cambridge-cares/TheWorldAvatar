from typing import Dict, TypeVar

K = TypeVar("K")
V = TypeVar("V")


class FrozenDict(Dict[K, V]):
    def __init__(self, args):
        super().__init__(args)

    def __setitem__(self, key: K, value: V) -> None:
        raise ValueError("Cannot perform SET on a frozen dict.")

    def __hash__(self):
        return hash(tuple(self.items()))
