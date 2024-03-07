import pytest

from services.translate.data_processing.ontozeolite.compact2verbose import (
    OZCompact2VerboseConverter,
)
from services.translate.sparql.graph_pattern import (
    OptionalClause,
    TriplePattern,
)
from services.translate.sparql.where_clause import WhereClause
from services.translate.sparql.query_form import SelectClause
from services.translate.sparql import SparqlQuery


class TestOZCompact2VerboseConverter:
    @pytest.mark.parametrize(
        "sparql_compact,expected",
        [
            (
                # SELECT ?UnitCell WHERE {
                #   ?Framework zeo:hasFrameworkCode "ABW" .
                #   ?Framework ocr:hasCrystalInformation/ocr:hasUnitCell ?UnitCell .
                # }
                SparqlQuery(
                    select_clause=SelectClause(vars=["?UnitCell"]),
                    where_clause=WhereClause(
                        [
                            TriplePattern.from_triple(
                                "?Framework", "zeo:hasFrameworkCode", "ABW"
                            ),
                            TriplePattern.from_triple(
                                "?Framework",
                                "ocr:hasCrystalInformation/ocr:hasUnitCell",
                                "?UnitCell",
                            ),
                        ]
                    ),
                ),
                # SELECT ?UnitCell ?a ?b ?c ?LengthsUnitLabel ?alpha ?beta ?gamma ?AnglesUnitLabel ?VolumeNumericalValue ?VolumeUnitLabel ?LatticeSystem ?SpaceGroupNumber_ITCr WHERE {
                #   ?Framework zeo:hasFrameworkCode "ABW" .
                #   ?Framework ocr:hasCrystalInformation/ocr:hasUnitCell ?UnitCell .
                #   ?UnitCell
                #       ocr:hasUnitCellLengths [
                #           ocr:hasVectorComponent [ ocr:hasComponentLabel "a" ; ocr:hasComponentValue ?a ] ,
                #                                  [ ocr:hasComponentLabel "b" ; ocr:hasComponentValue ?b ] ,
                #                                  [ ocr:hasComponentLabel "c" ; ocr:hasComponentValue ?c ] ;
                #           om:hasUnit/rdfs:label ?LengthsUnitLabel
                #       ] ;
                #       ocr:hasUnitCellAngles [
                #           ocr:hasVectorComponent [ ocr:hasComponentLabel "alpha" ; ocr:hasComponentValue ?alpha ] ,
                #                                  [ ocr:hasComponentLabel "beta" ; ocr:hasComponentValue ?beta ] ,
                #                                  [ ocr:hasComponentLabel "gamma" ; ocr:hasComponentValue ?gamma ] ;
                #          om:hasUnit/rdfs:label ?AnglesUnitLabel
                #       ] ;
                #       ocr:hasUnitCellVolume [
                #           om:hasNumericalValue ?VolumeNumericalValue ;
                #           om:hasUnit/rdfs:label ?VolumeUnitLabel
                #       ] .
                #   OPTIONAL {
                #       ?UnitCell ocr:hasLatticeSystem ?LatticeSystem .
                #   }
                #   OPTIONAL {
                #       ?UnitCell ocr:hasSymmetryNumber ?SpaceGroupNumber_ITCr .
                #   }
                # }
                SparqlQuery(
                    select_clause=SelectClause(
                        vars=[
                            "?UnitCell",
                            "?a",
                            "?b",
                            "?c",
                            "?LengthsUnitLabel",
                            "?alpha",
                            "?beta",
                            "?gamma",
                            "?AnglesUnitLabel",
                            "?VolumeNumericalValue",
                            "?VolumeUnitLabel",
                            "?LatticeSystem",
                            "?SpaceGroupNumber_ITCr",
                        ],
                        solution_modifier="DISTINCT",
                    ),
                    where_clause=WhereClause(
                        [
                            TriplePattern.from_triple(
                                "?Framework", "zeo:hasFrameworkCode", "ABW"
                            ),
                            TriplePattern.from_triple(
                                "?Framework",
                                "ocr:hasCrystalInformation/ocr:hasUnitCell",
                                "?UnitCell",
                            ),
                            TriplePattern(
                                "?UnitCell",
                                tails=[
                                    (
                                        "ocr:hasUnitCellLengths",
                                        '[ ocr:hasVectorComponent [ ocr:hasComponentLabel "a" ; ocr:hasComponentValue ?a ] , [ ocr:hasComponentLabel "b" ; ocr:hasComponentValue ?b ] , [ ocr:hasComponentLabel "c" ; ocr:hasComponentValue ?c ] ; om:hasUnit/rdfs:label ?LengthsUnitLabel ]',
                                    ),
                                    (
                                        "ocr:hasUnitCellAngles",
                                        '[ ocr:hasVectorComponent [ ocr:hasComponentLabel "alpha" ; ocr:hasComponentValue ?alpha ] , [ ocr:hasComponentLabel "beta" ; ocr:hasComponentValue ?beta ] , [ ocr:hasComponentLabel "gamma" ; ocr:hasComponentValue ?gamma ] ; om:hasUnit/rdfs:label ?AnglesUnitLabel ]',
                                    ),
                                    (
                                        "ocr:hasUnitCellVolume",
                                        "[ om:hasNumericalValue ?VolumeNumericalValue ; om:hasUnit/rdfs:label ?VolumeUnitLabel ]",
                                    ),
                                ],
                            ),
                            OptionalClause(
                                [
                                    TriplePattern.from_triple(
                                        "?UnitCell",
                                        "ocr:hasLatticeSystem",
                                        "?LatticeSystem",
                                    )
                                ]
                            ),
                            OptionalClause(
                                [
                                    TriplePattern.from_triple(
                                        "?UnitCell",
                                        "ocr:hasSymmetryNumber",
                                        "?SpaceGroupNumber_ITCr",
                                    )
                                ]
                            ),
                        ]
                    ),
                ),
            ),
        ],
    )
    def test_convert(self, sparql_compact, expected):
        converter = OZCompact2VerboseConverter()
        assert converter.convert(sparql_compact) == expected
