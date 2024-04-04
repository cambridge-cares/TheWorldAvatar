from typing import Dict, TypeVar

K = TypeVar("K")
V = TypeVar("V")


class FrozenDict(Dict[K, V]):
    def __init__(self, d: Dict[K, V]):
        super().__init__(d)

    def __setitem__(self, key, value):
        raise ValueError("Cannot mutate FrozenDict")

    def __hash__(self):
        return hash(tuple(sorted(self.items())))
