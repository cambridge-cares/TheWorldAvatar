from collections import defaultdict
from typing import TypeVar

_KT = TypeVar("_KT")
_VT = TypeVar("_VT")


class FrozenDict(dict[_KT, _VT]):
    @classmethod
    def from_any(cls, x):
        if isinstance(x, dict):
            return cls.from_dict(x)
        elif isinstance(x, list) or isinstance(x, tuple):
            return cls.from_list(x)
        else:
            return x

    @classmethod
    def from_list(cls, lst: list | tuple):
        return tuple(cls.from_any(x) for x in lst)

    @classmethod
    def from_dict(cls, obj: dict):
        return cls({k: cls.from_any(v) for k, v in obj.items()})

    def __init__(self, map: dict[_KT, _VT]):
        super().__init__(map)

    def __setitem__(self, key: _KT, value: _VT) -> None:
        raise ValueError("Cannot perform SET on a frozen dict.")

    def __repr__(self) -> str:
        return "frozendict({content})".format(content=super().__repr__())

    def __hash__(self):
        return hash(tuple(self.items()))


def listofdict2dictoflist(lst: list[dict]):
    key2lst = defaultdict(list)

    for obj in lst:
        for k, v in obj.items():
            key2lst[k].append(v)

    return {k: list(set(lst)) for k, lst in key2lst.items()}


def deep_update(source: dict, destination: dict):
    for key, value in source.items():
        if isinstance(value, dict):
            # get node or create one
            node = destination.setdefault(key, {})
            deep_update(value, node)
        else:
            destination[key] = value
