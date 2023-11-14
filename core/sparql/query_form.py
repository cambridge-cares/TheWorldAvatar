from dataclasses import dataclass, field
from typing import Tuple

from core.sparql.sparql_base import SparqlBase


@dataclass
class SelectClause(SparqlBase):
    vars: Tuple[str, ...] = field(default_factory=tuple)
    solution_modifier: str = ""

    def __str__(self):
        return "SELECT{solution_modifier} {vars}".format(
            solution_modifier=" " + self.solution_modifier
            if self.solution_modifier
            else "",
            vars=" ".join(self.vars),
        )

    def tolines(self):
        return [str(self)]
