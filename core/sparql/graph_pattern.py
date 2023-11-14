from dataclasses import dataclass, field
import itertools
from typing import Tuple

from core.sparql.sparql_base import SparqlBase


class GraphPattern(SparqlBase):
    pass


@dataclass
class ValuesClause(GraphPattern):
    var: str
    values: Tuple[str, ...] = field(default_factory=tuple)

    def __str__(self):
        return "VALUES {var} {{ {values} }}".format(
            var=self.var,
            values=" ".join(['"{val}"'.format(val=val) for val in self.values]),
        )


@dataclass
class FilterClause(GraphPattern):
    constraint: str

    def __str__(self):
        return "FILTER ( {constraint} )".format(constraint=self.constraint)


@dataclass
class TriplePattern(GraphPattern):
    subj: str
    # [(p1, o1), (p2, o2), ...]
    tails: Tuple[Tuple[str, str], ...] = field(default_factory=tuple)

    @classmethod
    def from_triple(cls, subj: str, predicate: str, obj: str):
        return cls(subj=subj, tails=[(predicate, obj)])

    def __str__(self):
        return "{subj} {tails} .".format(
            subj=self.subj,
            tails=" ; ".join(
                [
                    "{predicate} {obj}".format(predicate=predicate, obj=obj)
                    for predicate, obj in self.tails
                ]
            ),
        )


@dataclass
class OptionalClause(GraphPattern):
    graph_patterns: Tuple[GraphPattern, ...] = field(default_factory=tuple)

    def __str__(self):
        return "OPTIONAL {{\n{patterns}\n}}".format(
            patterns="\n".join(["  " + str(pattern) for pattern in self.graph_patterns])
        )

    def tolines(self):
        return list(
            itertools.chain.from_iterable(
                [
                    ["OPTIONAL {"],
                    ["  " + str(pattern) for pattern in self.graph_patterns],
                    ["}"],
                ]
            )
        )


@dataclass
class BindClause(GraphPattern):
    exprn: str
    var: str

    def __str__(self):
        return "BIND {expr} AS {var}".format(expr=self.exprn, var=self.var)
