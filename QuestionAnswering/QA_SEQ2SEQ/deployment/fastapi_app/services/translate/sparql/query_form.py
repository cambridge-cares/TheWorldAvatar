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

    @classmethod
    def extract(cls, sparql: str):
        """sparql: SELECT ?x ((EXRP) AS ?y)..."""
        sparql = sparql.lstrip()
        assert sparql.startswith("SELECT")
        sparql = sparql[len("SELECT") :]
        vars = []
        while True:
            sparql = sparql.lstrip()
            if sparql.startswith("?"):
                var, sparql = sparql.split(maxsplit=1)
                vars.append(var)
            elif sparql.startswith("("):
                open_brack_count = 1
                ptr = 1
                while open_brack_count > 0 and ptr < len(sparql):
                    if sparql[ptr] == "(":
                        open_brack_count += 1
                    elif sparql[ptr] == ")":
                        open_brack_count -= 1
                    ptr += 1
                vars.append(sparql[:ptr])
                sparql = sparql[ptr:]
            else:
                break

        return cls(vars), sparql
