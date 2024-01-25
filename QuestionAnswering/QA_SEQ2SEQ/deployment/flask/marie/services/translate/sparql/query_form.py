from dataclasses import dataclass
from typing import Iterable, Tuple

from .sparql_base import SparqlBase


@dataclass(order=True, frozen=True)
class SelectClause(SparqlBase):
    vars: Tuple[str, ...]
    solution_modifier: str = ""

    def __init__(self, vars: Iterable[str], solution_modifier: str = ""):
        object.__setattr__(self, "vars", tuple(vars))
        object.__setattr__(self, "solution_modifier", solution_modifier)

    def __str__(self):
        return "SELECT{solution_modifier} {vars}".format(
            solution_modifier=" " + self.solution_modifier
            if self.solution_modifier
            else "",
            vars=" ".join(self.vars),
        )
