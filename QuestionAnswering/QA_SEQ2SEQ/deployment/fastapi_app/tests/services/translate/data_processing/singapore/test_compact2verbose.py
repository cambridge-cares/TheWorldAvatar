import pytest

from services.translate.sparql.graph_pattern import ServicePattern, TriplePattern
from services.translate.sparql.where_clause import WhereClause
from services.translate.sparql.query_form import SelectClause
from services.translate.sparql import SparqlQuery
from services.translate.data_processing.singapore.compact2verbose import (
    SgCompact2VerboseConverter,
)


class TestSgCompact2VerboseConverter:
    ontop_endpoint = "http://localhost:8000/ontop/sparql"

    @pytest.mark.parametrize(
        "sparql_compact,expected",
        [
            (
                # SELECT (COUNT(?Plot) AS ?PlotCount) WHERE { ?Plot ozng:hasLandUseType/a ozng:Commercial  }
                SparqlQuery(
                    select_clause=SelectClause(vars=["(COUNT(?Plot) AS ?PlotCount)"]),
                    where_clause=WhereClause(
                        graph_patterns=[
                            TriplePattern.from_triple(
                                "?Plot", "ozng:hasLandUseType/a", "ozng:Commercial"
                            )
                        ]
                    ),
                ),
                # SELECT DISTINCT (COUNT(?Plot) AS ?PlotCount) WHERE {
                #     ?LandUseType a ozng:Commercial .
                #     SERVICE <http://localhost:8000/ontop/sparql> {
                #         ?Plot a oplt:Plot .
                #         ?Plot ozng:hasLandUseType ?LandUseType .
                #     }
                # }
                SparqlQuery(
                    select_clause=SelectClause(
                        vars=["(COUNT(?Plot) AS ?PlotCount)"],
                        solution_modifier="DISTINCT",
                    ),
                    where_clause=WhereClause(
                        graph_patterns=[
                            TriplePattern.from_triple(
                                "?LandUseType", "a", "ozng:Commercial"
                            ),
                            ServicePattern(
                                endpoint="http://localhost:8000/ontop/sparql",
                                graph_patterns=[
                                    TriplePattern.from_triple(
                                        "?Plot", "a", "oplt:Plot"
                                    ),
                                    TriplePattern.from_triple(
                                        "?Plot", "ozng:hasLandUseType", "?LandUseType"
                                    ),
                                ],
                            ),
                        ]
                    ),
                ),
            ),
            (
                # SELECT ?GrossPlotRatio ?IsAwaitingDetailedGPREvaluation WHERE {
                #   ?Plot ^oplnrgl:appliesTo/oplnrgl:allowsGrossPlotRatio/om:hasValue ?GrossPlotRatio .
                #   ?Plot oplnrgl:isAwaitingDetailedGPREvaluation ?IsAwaitingDetailedGPREvaluation .
                # }
                SparqlQuery(
                    select_clause=SelectClause(
                        vars=["?GrossPlotRatio", "?IsAwaitingDetailedGPREvaluation"]
                    ),
                    where_clause=WhereClause(
                        graph_patterns=[
                            TriplePattern.from_triple(
                                "?Plot",
                                "^oplnrgl:appliesTo/oplnrgl:allowsGrossPlotRatio/om:hasValue",
                                "?GrossPlotRatio",
                            ),
                            TriplePattern.from_triple(
                                "?Plot",
                                "oplnrgl:isAwaitingDetailedGPREvaluation",
                                "?IsAwaitingDetailedGPREvaluation",
                            ),
                        ]
                    ),
                ),
                # SELECT DISTINCT ?GrossPlotRatio ?IsAwaitingDetailedGPREvaluation ?GrossPlotRatioNumericalValue ?GrossPlotRatioUnit WHERE {
                #   SERVICES <http://localhost:8000/ontop/sparql> {
                #     ?Plot a oplt:Plot .
                #     ?Plot ^oplnrgl:appliesTo/oplnrgl:allowsGrossPlotRatio/om:hasValue ?GrossPlotRatio .
                #     ?GrossPlotRatio om:hasNumericalValue ?GrossPlotRatioNumericalValue ; om:hasUnit ?GrossPlotRatioUnit .
                #     ?Plot oplnrgl:isAwaitingDetailedGPREvaluation ?IsAwaitingDetailedGPREvaluation .
                #   }
                # }
                SparqlQuery(
                    select_clause=SelectClause(
                        vars=[
                            "?GrossPlotRatio",
                            "?IsAwaitingDetailedGPREvaluation",
                            "?GrossPlotRatioNumericalValue",
                            "?GrossPlotRatioUnit",
                        ],
                        solution_modifier="DISTINCT",
                    ),
                    where_clause=WhereClause(
                        graph_patterns=[
                            ServicePattern(
                                endpoint="http://localhost:8000/ontop/sparql",
                                graph_patterns=[
                                    TriplePattern.from_triple(
                                        "?Plot", "a", "oplt:Plot"
                                    ),
                                    TriplePattern.from_triple(
                                        "?Plot",
                                        "^oplnrgl:appliesTo/oplnrgl:allowsGrossPlotRatio/om:hasValue",
                                        "?GrossPlotRatio",
                                    ),
                                    TriplePattern(
                                        subj="?GrossPlotRatio",
                                        tails=[
                                            (
                                                "om:hasNumericalValue",
                                                "?GrossPlotRatioNumericalValue",
                                            ),
                                            ("om:hasUnit", "?GrossPlotRatioUnit"),
                                        ],
                                    ),
                                    TriplePattern.from_triple(
                                        "?Plot",
                                        "oplnrgl:isAwaitingDetailedGPREvaluation",
                                        "?IsAwaitingDetailedGPREvaluation",
                                    ),
                                ],
                            )
                        ]
                    ),
                ),
            ),
        ],
    )
    def test_convert(self, sparql_compact, expected):
        converter = SgCompact2VerboseConverter(ontop_endpoint=self.ontop_endpoint)
        assert converter.convert(sparql_compact) == expected
