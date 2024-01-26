from dataclasses import dataclass
from typing import Tuple

from .graph_pattern import (
    FilterClause,
    GraphPattern,
    TriplePattern,
    ValuesClause,
)
from .sparql_base import SparqlBase


@dataclass
class WhereClause(SparqlBase):
    graph_patterns: Tuple[GraphPattern, ...]

    def __post_init__(self):
        if not isinstance(self.graph_patterns, tuple):
            object.__setattr__(self, "graph_patterns", tuple(self.graph_patterns))

    def __str__(self):
        return "WHERE {{\n{group_graph_pattern}\n}}".format(
            group_graph_pattern="\n".join(
                [
                    "  " + line
                    for pattern in self.graph_patterns
                    for line in pattern.tolines()
                ]
            ),
        )

    @classmethod
    def extract(cls, sparql_fragment: str):
        """sparql_fragment: WHERE { ... }"""
        sparql_fragment = sparql_fragment.lstrip()
        assert sparql_fragment.startswith("WHERE"), sparql_fragment
        sparql_fragment = sparql_fragment[len("WHERE") :].lstrip()
        assert sparql_fragment.startswith("{")
        sparql_fragment = sparql_fragment[1:].lstrip()

        graph_patterns = []
        while sparql_fragment and not sparql_fragment.startswith("}"):
            if sparql_fragment.startswith("VALUES"):
                pattern, sparql_fragment = ValuesClause.extract(sparql_fragment)
            elif sparql_fragment.startswith("FILTER"):
                pattern, sparql_fragment = FilterClause.extract(sparql_fragment)
            else:
                pattern, sparql_fragment = TriplePattern.extract(sparql_fragment)
            graph_patterns.append(pattern)
            sparql_fragment = sparql_fragment.lstrip()

        assert sparql_fragment.startswith("}"), sparql_fragment
        return cls(graph_patterns), sparql_fragment[1:]
