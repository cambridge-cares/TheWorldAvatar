from dataclasses import dataclass
import itertools
from typing import Iterable, Tuple

from core.sparql.sparql_base import SparqlBase


class GraphPattern(SparqlBase):
    pass


@dataclass(order=True, frozen=True)
class ValuesClause(GraphPattern):
    var: str
    values: Tuple[str, ...]

    def __init__(self, var: str, values: Iterable[str]):
        object.__setattr__(self, "var", var)
        object.__setattr__(self, "values", tuple(values))

    def __str__(self):
        return "VALUES {var} {{ {values} }}".format(
            var=self.var,
            values=" ".join(
                [
                    '"{val}"'.format(val=val) if ":" not in val else val
                    for val in self.values
                ]
            ),
        )


@dataclass(order=True, frozen=True)
class FilterClause(GraphPattern):
    constraint: str

    def __str__(self):
        return "FILTER ( {constraint} )".format(constraint=self.constraint)


@dataclass(order=True, frozen=True)
class TriplePattern(GraphPattern):
    subj: str
    # [(p1, o1), (p2, o2), ...]
    tails: Tuple[Tuple[str, str], ...]

    def __init__(self, subj: str, tails: Iterable[Tuple[str, str]]):
        object.__setattr__(self, "subj", subj)
        object.__setattr__(self, "tails", tuple(tails))

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


@dataclass(order=True, frozen=True)
class OptionalClause(GraphPattern):
    graph_patterns: Tuple[GraphPattern, ...]

    def __init__(self, graph_patterns: Iterable[GraphPattern]):
        object.__setattr__(self, "graph_patterns", tuple(graph_patterns))

    def __str__(self):
        return "OPTIONAL {{\n{patterns}\n}}".format(
            patterns="\n".join(["  " + str(pattern) for pattern in self.graph_patterns])
        )

    def tolines(self):
        return list(
            itertools.chain.from_iterable(
                [
                    ["OPTIONAL {"],
                    [
                        "  " + str(line)
                        for pattern in self.graph_patterns
                        for line in pattern.tolines()
                    ],
                    ["}"],
                ]
            )
        )


@dataclass(order=True, frozen=True)
class BindClause(GraphPattern):
    exprn: str
    var: str

    def __str__(self):
        return "BIND ({expr} AS {var})".format(expr=self.exprn, var=self.var)
