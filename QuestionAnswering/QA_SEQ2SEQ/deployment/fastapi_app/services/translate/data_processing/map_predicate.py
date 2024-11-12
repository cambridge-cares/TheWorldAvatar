from typing import Dict

from services.translate.sparql.where_clause import WhereClause
from services.translate.sparql.graph_pattern import TriplePattern
from services.translate.sparql import SparqlQuery


class PredicateMapper:
    def __init__(self, mappings: Dict[str, str]):
        self.mappings = mappings

    def map(self, query: SparqlQuery):
        graph_patterns = []
        for pattern in query.where_clause.graph_patterns:
            if isinstance(pattern, TriplePattern):
                tails = []
                for pred, obj in pattern.tails:
                    for old, new in self.mappings.items():
                        pred = pred.replace(old, new)
                    tails.append((pred, obj))
                graph_patterns.append(TriplePattern(pattern.subj, tails))
            else:
                graph_patterns.append(pattern)
        return SparqlQuery(
            select_clause=query.select_clause,
            where_clause=WhereClause(graph_patterns=graph_patterns),
            solution_modifier=query.solution_modifier
        )