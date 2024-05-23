from typing import Dict, TypeVar

_KT = TypeVar("_KT")
_VT = TypeVar("_VT")


class FrozenDict(Dict[_KT, _VT]):
    def __init__(self, map: Dict[_KT, _VT]):
        super().__init__(map)

    def __setitem__(self, key: _KT, value: _VT) -> None:
        raise ValueError("Cannot perform SET on a frozen dict.")

    def __hash__(self):
        return hash(tuple(self.items()))
