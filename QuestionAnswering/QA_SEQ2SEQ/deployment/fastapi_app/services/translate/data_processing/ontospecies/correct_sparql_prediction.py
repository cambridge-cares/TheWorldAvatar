from services.translate.triton_client.feature_extraction_client import (
    IFeatureExtractionClient,
)
from services.translate.sparql import SparqlQuery
from services.translate.sparql.graph_pattern import TriplePattern, ValuesClause
from services.translate.sparql.where_clause import WhereClause
from .correct_predicate import OSPredicateCorrector
from .correct_span import OSSpanCorrector


class OSSparqlPredictionCorrector:
    def __init__(self, feature_extraction_client: IFeatureExtractionClient):
        self.predicate_corrector = OSPredicateCorrector(feature_extraction_client)
        self.span_corrector = OSSpanCorrector()

    def correct_values_clause(self, values_clause: ValuesClause, nlq: str):
        values = []
        for value in values_clause.values:
            value = self.span_corrector.correct(nlq, value)
            values.append(value)
        return ValuesClause(values_clause.var, values)

    def correct_triple_pattern(self, triple_pattern: TriplePattern):
        tails = []
        for predicate, obj in triple_pattern.tails:
            predicate = self.predicate_corrector.correct(predicate)
            tails.append((predicate, obj))
        return TriplePattern(subj=triple_pattern.subj, tails=tails)

    def correct(self, sparql: SparqlQuery, nlq: str):
        graph_patterns = []
        for pattern in sparql.where_clause.graph_patterns:
            if isinstance(pattern, ValuesClause):
                pattern = self.correct_values_clause(pattern, nlq)
            elif isinstance(pattern, TriplePattern):
                pattern = self.correct_triple_pattern(pattern)
            graph_patterns.append(pattern)

        return SparqlQuery(
            select_clause=sparql.select_clause,
            where_clause=WhereClause(graph_patterns),
            solution_modifier=sparql.solution_modifier,
        )
