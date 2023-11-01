from dataclasses import dataclass
from typing import List, Tuple

from core.sparql.sparql_base import SparqlBase

class GraphPattern(SparqlBase):
    pass


@dataclass
class ValuesClause(GraphPattern):
    var: str
    values: List[str]


@dataclass
class FilterClause(GraphPattern):
    constraint: str


@dataclass
class TriplePattern(GraphPattern):
    subj: str
    tails: List[Tuple[str, str]]  # [(p1, o1), (p2, o2), ...]

    @classmethod
    def from_triple(cls, subj: str, predicate: str, obj: str):
        return cls(subj=subj, tails=[(predicate, obj)])


@dataclass
class OptionalClause(GraphPattern):
    graph_patterns: List[GraphPattern]


@dataclass
class BindClause(GraphPattern):
    exprn: str
    var: str