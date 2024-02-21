from dataclasses import dataclass
from typing import Optional, Tuple

from .exceptions import SparqlParseError
from .sparql_base import SparqlBase


@dataclass
class GroupClause(SparqlBase):
    vars: Tuple[str, ...]

    def __post_init__(self):
        if not isinstance(self.vars, tuple):
            object.__setattr__(self, "vars", tuple(self.vars))

    def __str__(self):
        return "GROUP BY {vars}".format(vars=" ".join(self.vars))


@dataclass
class OrderClause(SparqlBase):
    vars: Tuple[str, ...]

    def __post_init__(self):
        if not isinstance(self.vars, tuple):
            object.__setattr__(self, "vars", tuple(self.vars))

    def __str__(self):
        return "ORDER BY {vars}".format(vars=" ".join(self.vars))


@dataclass
class LimitClause(SparqlBase):
    num: int

    def __str__(self):
        return "LIMIT {num}".format(num=self.num)


@dataclass
class SolutionModifier(SparqlBase):
    group_clause: Optional[GroupClause] = None
    order_clause: Optional[OrderClause] = None
    limit_clause: Optional[LimitClause] = None

    def __str__(self):
        clauses = [self.group_clause, self.order_clause, self.limit_clause]
        clauses = [str(x) for x in clauses if x]
        return "\n".join(clauses)

    @classmethod
    def extract(cls, sparql_fragment: str):
        sparql_fragment = sparql_fragment.lstrip()
        if sparql_fragment.startswith("GROUP BY"):
            sparql_fragment = sparql_fragment[len("GROUP BY") :].lstrip()
            vars = []
            while sparql_fragment.startswith("?"):
                var, sparql_fragment = sparql_fragment.split(maxsplit=1)
                vars.append(var)
            if not vars:
                raise SparqlParseError(sparql_fragment)
            group_clause = GroupClause(vars)
        else:
            group_clause = None

        sparql_fragment = sparql_fragment.lstrip()
        if sparql_fragment.startswith("ORDER BY"):
            sparql_fragment = sparql_fragment[len("ORDER BY") :].lstrip()
            vars = []
            while True:
                if sparql_fragment.startswith("?"):
                    var, sparql_fragment = sparql_fragment.split(maxsplit=1)
                elif sparql_fragment.startswith("DESC"):
                    ptr = len("DESC")
                    while sparql_fragment[ptr].isspace():
                        ptr += 1
                    
                    if sparql_fragment[ptr] != "(":
                        raise SparqlParseError(sparql_fragment)
            
                    ptr = sparql_fragment.find(")", ptr)
                    if ptr < 0:
                        raise SparqlParseError(sparql_fragment)
                    var = sparql_fragment[: ptr + 1]
                    sparql_fragment = sparql_fragment[ptr + 1 :]
                else:
                    break
                vars.append(var)
                sparql_fragment = sparql_fragment.lstrip()
            if not vars:
                raise SparqlParseError(sparql_fragment)
            order_clause = OrderClause(vars)
        else:
            order_clause = None

        sparql_fragment = sparql_fragment.lstrip()
        if sparql_fragment.startswith("LIMIT"):
            sparql_fragment = sparql_fragment[len("LIMIT") :].lstrip()
            items = sparql_fragment.split(maxsplit=1)
            if len(items) == 2:
                num, sparql_fragment = items
                limit_clause = LimitClause(int(num))
            elif len(items) == 1:
                limit_clause = LimitClause(int(items[0]))
                sparql_fragment = ""
            else:
                raise SparqlParseError(sparql_fragment)
        else:
            limit_clause = None

        if not all((group_clause, order_clause, limit_clause)):
            solution_modifier = None
        else:
            solution_modifier = cls(
                group_clause=group_clause,
                order_clause=order_clause,
                limit_clause=limit_clause,
            )

        return solution_modifier, sparql_fragment
