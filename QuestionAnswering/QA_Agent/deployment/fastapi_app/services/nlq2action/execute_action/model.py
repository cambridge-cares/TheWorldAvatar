from typing import Any, Dict
from pydantic.dataclasses import dataclass


class ActionBase:
    pass


@dataclass
class SparqlAction(ActionBase):
    namespace: str
    query: str


@dataclass
class FuncAction(ActionBase):
    name: str
    args: Dict[str, Any]
