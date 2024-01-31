from dataclasses import dataclass
from typing import Iterable, Tuple
from .sparql_base import SparqlBase


@dataclass(order=True, frozen=True)
class GroupByClause(SparqlBase):
    vars: Tuple[str]

    def __init__(self, vars: Iterable[str]):
        object.__setattr__(self, "vars", tuple(vars))

    def __str__(self):
        return "GROUP BY {vars}".format(vars=" ".join(self.vars))
