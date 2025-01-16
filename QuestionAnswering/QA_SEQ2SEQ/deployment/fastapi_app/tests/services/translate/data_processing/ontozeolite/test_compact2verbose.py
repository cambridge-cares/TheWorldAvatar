import pytest

from services.translate.sparql import SparqlQuery
from services.translate.sparql.query_form import SelectClause
from services.translate.sparql.where_clause import WhereClause
from services.translate.sparql.graph_pattern import (
    FilterClause,
    OptionalClause,
    ServicePattern,
    TriplePattern,
)
from services.translate.sparql.constraint import BrackettedExpression, NotExistsFunc
from services.translate.data_processing.ontozeolite.compact2verbose import (
    OZCompact2VerboseConverter,
)


class TestOZCompact2VerboseConverter:
    ONTOSPECIES_ENDPOINT_TEST = "http://ontospecies.com/sparql"

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
                # SELECT ?UnitCell ?a ?b ?c ?LengthsUnit ?alpha ?beta ?gamma ?AnglesUnit ?VolumeNumericalValue ?VolumeUnit ?LatticeSystem ?SpaceGroupNumber_ITCr WHERE {
                #   ?Framework zeo:hasFrameworkCode "ABW" .
                #   ?Framework ocr:hasCrystalInformation/ocr:hasUnitCell ?UnitCell .
                #   ?UnitCell
                #       ocr:hasUnitCellLengths [
                #           ocr:hasVectorComponent [ ocr:hasComponentLabel "a" ; ocr:hasComponentValue ?a ] ,
                #                                  [ ocr:hasComponentLabel "b" ; ocr:hasComponentValue ?b ] ,
                #                                  [ ocr:hasComponentLabel "c" ; ocr:hasComponentValue ?c ] ;
                #           om:hasUnit ?LengthsUnit
                #       ] ;
                #       ocr:hasUnitCellAngles [
                #           ocr:hasVectorComponent [ ocr:hasComponentLabel "alpha" ; ocr:hasComponentValue ?alpha ] ,
                #                                  [ ocr:hasComponentLabel "beta" ; ocr:hasComponentValue ?beta ] ,
                #                                  [ ocr:hasComponentLabel "gamma" ; ocr:hasComponentValue ?gamma ] ;
                #          om:hasUnit ?AnglesUnit
                #       ] ;
                #       ocr:hasUnitCellVolume [
                #           om:hasNumericalValue ?VolumeNumericalValue ;
                #           om:hasUnit ?VolumeUnit
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
                            "?LengthsUnit",
                            "?alpha",
                            "?beta",
                            "?gamma",
                            "?AnglesUnit",
                            "?VolumeNumericalValue",
                            "?VolumeUnit",
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
                                        '[ ocr:hasVectorComponent [ ocr:hasComponentLabel "a" ; ocr:hasComponentValue ?a ] , [ ocr:hasComponentLabel "b" ; ocr:hasComponentValue ?b ] , [ ocr:hasComponentLabel "c" ; ocr:hasComponentValue ?c ] ; om:hasUnit ?LengthsUnit ]',
                                    ),
                                    (
                                        "ocr:hasUnitCellAngles",
                                        '[ ocr:hasVectorComponent [ ocr:hasComponentLabel "alpha" ; ocr:hasComponentValue ?alpha ] , [ ocr:hasComponentLabel "beta" ; ocr:hasComponentValue ?beta ] , [ ocr:hasComponentLabel "gamma" ; ocr:hasComponentValue ?gamma ] ; om:hasUnit ?AnglesUnit ]',
                                    ),
                                    (
                                        "ocr:hasUnitCellVolume",
                                        "[ om:hasNumericalValue ?VolumeNumericalValue ; om:hasUnit ?VolumeUnit ]",
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
            (
                # SELECT ?Framework WHERE {
                #   ?Framework zeo:hasTopologicalProperties/zeo:hasSpecificAccessibleArea/om:hasNumericalValue ?SpecificAccessibleAreaNumericalValue.
                #   FILTER ( ?SpecificAccessibleAreaNumericalValue <= 100 )
                # }
                SparqlQuery(
                    select_clause=SelectClause(vars=["?Framework"]),
                    where_clause=WhereClause(
                        [
                            TriplePattern.from_triple(
                                "?Framework",
                                "zeo:hasTopologicalProperties/zeo:hasSpecificAccessibleArea/om:hasNumericalValue",
                                "?SpecificAccessibleAreaNumericalValue",
                            ),
                            FilterClause(
                                BrackettedExpression(
                                    "?SpecificAccessibleAreaNumericalValue <= 100"
                                )
                            ),
                        ]
                    ),
                ),
                # SELECT DISTINCT ?Framework ?FrameworkCode ?SpecificAccessibleAreaNumericalValue ?SpecificAccessibleAreaUnit WHERE {
                #   ?Framework zeo:hasFrameworkCode ?FrameworkCode .
                #   ?Framework zeo:hasTopologicalProperties/zeo:hasSpecificAccessibleArea ?SpecificAccessibleArea .
                #   ?SpecificAccessibleArea om:hasNumericalValue ?SpecificAccessibleAreaNumericalValue ; om:hasUnit ?SpecificAccessibleAreaUnit .
                #   FILTER ( ?SpecificAccessibleAreaNumericalValue <= 100 )
                # }
                SparqlQuery(
                    select_clause=SelectClause(
                        vars=[
                            "?Framework",
                            "?FrameworkCode",
                            "?SpecificAccessibleAreaNumericalValue",
                            "?SpecificAccessibleAreaUnit",
                        ],
                        solution_modifier="DISTINCT",
                    ),
                    where_clause=WhereClause(
                        [
                            TriplePattern.from_triple(
                                "?Framework", "zeo:hasFrameworkCode", "?FrameworkCode"
                            ),
                            TriplePattern.from_triple(
                                "?Framework",
                                "zeo:hasTopologicalProperties/zeo:hasSpecificAccessibleArea",
                                "?SpecificAccessibleArea",
                            ),
                            TriplePattern(
                                subj="?SpecificAccessibleArea",
                                tails=[
                                    (
                                        "om:hasNumericalValue",
                                        "?SpecificAccessibleAreaNumericalValue",
                                    ),
                                    (
                                        "om:hasUnit",
                                        "?SpecificAccessibleAreaUnit",
                                    ),
                                ],
                            ),
                            FilterClause(
                                BrackettedExpression(
                                    "?SpecificAccessibleAreaNumericalValue <= 100"
                                )
                            ),
                        ]
                    ),
                ),
            ),
            (
                # SELECT ?Framework WHERE {
                #   ?Framework zeo:hasZeoliticMaterial ?Material .
                #   ?Material zeo:hasFrameworkComponent/rdfs:label "Si" .
                #   ?Material zeo:hasFrameworkComponent/rdfs:label "Oxygen" .
                # }
                SparqlQuery(
                    select_clause=SelectClause(["?Framework"]),
                    where_clause=WhereClause(
                        [
                            TriplePattern.from_triple(
                                "?Framework", "zeo:hasZeoliticMaterial", "?Material"
                            ),
                            TriplePattern.from_triple(
                                "?Material",
                                "zeo:hasFrameworkComponent/rdfs:label",
                                '"Si"',
                            ),
                            TriplePattern.from_triple(
                                "?Material",
                                "zeo:hasFrameworkComponent/rdfs:label",
                                '"Oxygen"',
                            ),
                        ]
                    ),
                ),
                # SELECT DISTINCT ?Framework ?FrameworkCode ?Element1 ?Element2 WHERE {
                #   ?Framework zeo:hasFrameworkCode ?FrameworkCode .
                #   ?Framework zeo:hasZeoliticMaterial ?Material .
                #   ?Material zeo:hasFrameworkComponent ?Element1 .
                #   ?Material zeo:hasFrameworkComponent ?Element2 .
                #   SERVICE <http://ontospecies.com/sparql> {
                #     ?Element1 (os:hasElementSymbol|os:hasElementName)/os:value "Si" .
                #     ?Element2 (os:hasElementSymbol|os:hasElementName)/os:value "Oxygen" .
                #   }
                # }
                SparqlQuery(
                    select_clause=SelectClause(
                        vars=["?Framework", "?FrameworkCode", "?Element1", "?Element2"],
                        solution_modifier="DISTINCT",
                    ),
                    where_clause=WhereClause(
                        [
                            TriplePattern.from_triple(
                                "?Framework", "zeo:hasFrameworkCode", "?FrameworkCode"
                            ),
                            TriplePattern.from_triple(
                                "?Framework", "zeo:hasZeoliticMaterial", "?Material"
                            ),
                            TriplePattern.from_triple(
                                "?Material", "zeo:hasFrameworkComponent", "?Element1"
                            ),
                            TriplePattern.from_triple(
                                "?Material", "zeo:hasFrameworkComponent", "?Element2"
                            ),
                            ServicePattern(
                                endpoint="http://ontospecies.com/sparql",
                                graph_patterns=[
                                    TriplePattern.from_triple(
                                        "?Element1",
                                        "(os:hasElementSymbol|os:hasElementName)/os:value",
                                        '"Si"',
                                    ),
                                    TriplePattern.from_triple(
                                        "?Element2",
                                        "(os:hasElementSymbol|os:hasElementName)/os:value",
                                        '"Oxygen"',
                                    ),
                                ],
                            ),
                        ]
                    ),
                ),
            ),
            (
                # SELECT ?Framework WHERE {
                #   ?Framework zeo:hasZeoliticMaterial ?Material .
                #   ?Material zeo:hasFrameworkComponentOnly/rdfs:label "Ga" .
                #   ?Material zeo:hasFrameworkComponentOnly/rdfs:label "Ge" .
                #   ?Material zeo:hasFrameworkComponentOnly/rdfs:label "S" .
                # }
                SparqlQuery(
                    select_clause=SelectClause(vars=["?Framework"]),
                    where_clause=WhereClause(
                        [
                            TriplePattern.from_triple(
                                "?Framework", "zeo:hasZeoliticMaterial", "?Material"
                            ),
                            TriplePattern.from_triple(
                                "?Material",
                                "zeo:hasFrameworkComponentOnly/rdfs:label",
                                '"Ga"',
                            ),
                            TriplePattern.from_triple(
                                "?Material",
                                "zeo:hasFrameworkComponentOnly/rdfs:label",
                                '"Ge"',
                            ),
                            TriplePattern.from_triple(
                                "?Material",
                                "zeo:hasFrameworkComponentOnly/rdfs:label",
                                '"S"',
                            ),
                        ]
                    ),
                ),
                # SELECT DISTINCT ?Framework ?FrameworkCode ?Element1 ?Element2 ?Element3 WHERE {
                #   ?Framework zeo:hasFrameworkCode ?FrameworkCode .
                #   ?Framework zeo:hasZeoliticMaterial ?Material .
                #   ?Material zeo:hasFrameworkComponent ?Element1 .
                #   ?Material zeo:hasFrameworkComponent ?Element2 .
                #   ?Material zeo:hasFrameworkComponent ?Element3 .
                #   SERVICE <http://ontospecies.com/sparql> {
                #     ?Element1 (os:hasElementSymbol|os:hasElementName)/os:value "Ga" .
                #     ?Element2 (os:hasElementSymbol|os:hasElementName)/os:value "Ge" .
                #     ?Element3 (os:hasElementSymbol|os:hasElementName)/os:value "S" .
                #     ?Oxygen os:hasElementSymbol/os:value "O" .
                #   }
                #   FILTER NOT EXISTS {
                #     ?Material zeo:hasFrameworkComponent ?ElementExclude .
                #     FILTER ( ?ElementExclude NOT IN ( ?Element1, ?Element2, ?Element3, ?Oxygen ) )
                #   }
                # }
                SparqlQuery(
                    select_clause=SelectClause(
                        vars=[
                            "?Framework",
                            "?FrameworkCode",
                            "?Element1",
                            "?Element2",
                            "?Element3",
                        ],
                        solution_modifier="DISTINCT",
                    ),
                    where_clause=WhereClause(
                        [
                            TriplePattern.from_triple(
                                "?Framework", "zeo:hasFrameworkCode", "?FrameworkCode"
                            ),
                            TriplePattern.from_triple(
                                "?Framework", "zeo:hasZeoliticMaterial", "?Material"
                            ),
                            TriplePattern.from_triple(
                                "?Material", "zeo:hasFrameworkComponent", "?Element1"
                            ),
                            TriplePattern.from_triple(
                                "?Material", "zeo:hasFrameworkComponent", "?Element2"
                            ),
                            TriplePattern.from_triple(
                                "?Material", "zeo:hasFrameworkComponent", "?Element3"
                            ),
                            FilterClause(
                                NotExistsFunc(
                                    [
                                        TriplePattern.from_triple(
                                            "?Material",
                                            "zeo:hasFrameworkComponent",
                                            "?ElementExclude",
                                        ),
                                        FilterClause(
                                            BrackettedExpression(
                                                "?ElementExclude NOT IN ( ?Element1, ?Element2, ?Element3, ?Oxygen )"
                                            )
                                        ),
                                    ]
                                )
                            ),
                            ServicePattern(
                                endpoint="http://ontospecies.com/sparql",
                                graph_patterns=[
                                    TriplePattern.from_triple(
                                        "?Element1",
                                        "(os:hasElementSymbol|os:hasElementName)/os:value",
                                        '"Ga"',
                                    ),
                                    TriplePattern.from_triple(
                                        "?Element2",
                                        "(os:hasElementSymbol|os:hasElementName)/os:value",
                                        '"Ge"',
                                    ),
                                    TriplePattern.from_triple(
                                        "?Element3",
                                        "(os:hasElementSymbol|os:hasElementName)/os:value",
                                        '"S"',
                                    ),
                                    TriplePattern.from_triple(
                                        "?Oxygen",
                                        "os:hasElementSymbol/os:value",
                                        '"O"',
                                    ),
                                ],
                            ),
                        ]
                    ),
                ),
            ),
            (
                # SELECT ?Material WHERE {
                #   ?Material zeo:hasGuestCompound/rdfs:label "H2O" .
                #   ?Material zeo:hasGuestCompound/rdfs:label "Cl-"
                # }
                SparqlQuery(
                    select_clause=SelectClause(vars=["?Material"]),
                    where_clause=WhereClause(
                        [
                            TriplePattern.from_triple(
                                "?Material", "zeo:hasGuestCompound/rdfs:label", '"H2O"'
                            ),
                            TriplePattern.from_triple(
                                "?Material", "zeo:hasGuestCompound/rdfs:label", '"Cl-"'
                            ),
                        ]
                    ),
                ),
                # SELECT DISTINCT ?Material ?ChemicalFormula ?Guest1 ?Guest2 WHERE {
                #   ?Material zeo:hasChemicalFormula ?ChemicalFormula .
                #   ?Material zeo:hasGuestCompound ?Guest1 .
                #   ?Material zeo:hasGuestCompound ?Guest2 .
                #   SERVICE <http://ontospecies.com/sparql> {
                #     ?Guest1 ?hasIdentifier1 [ a/rdfs:subClassOf os:Identifier ; os:value "H2O" ] .
                #     ?Guest2 ?hasIdentifier2 [ a/rdfs:subclassOf os:Identifier ; os:value "Cl-" ] .
                #   }
                # }
                SparqlQuery(
                    select_clause=SelectClause(
                        vars=["?Material", "?ChemicalFormula", "?Guest1", "?Guest2"],
                        solution_modifier="DISTINCT",
                    ),
                    where_clause=WhereClause(
                        [
                            TriplePattern.from_triple(
                                "?Material",
                                "zeo:hasChemicalFormula",
                                "?ChemicalFormula",
                            ),
                            TriplePattern.from_triple(
                                "?Material",
                                "zeo:hasGuestCompound",
                                "?Guest1",
                            ),
                            TriplePattern.from_triple(
                                "?Material",
                                "zeo:hasGuestCompound",
                                "?Guest2",
                            ),
                            ServicePattern(
                                endpoint="http://ontospecies.com/sparql",
                                graph_patterns=[
                                    TriplePattern.from_triple(
                                        "?Guest1",
                                        "?hasIdentifier1",
                                        '[ a/rdfs:subClassOf os:Identifier ; os:value "H2O" ]',
                                    ),
                                    TriplePattern.from_triple(
                                        "?Guest2",
                                        "?hasIdentifier2",
                                        '[ a/rdfs:subClassOf os:Identifier ; os:value "Cl-" ]',
                                    ),
                                ],
                            ),
                        ]
                    ),
                ),
            ),
        ],
    )
    def test_convert(self, sparql_compact, expected):
        converter = OZCompact2VerboseConverter(self.ONTOSPECIES_ENDPOINT_TEST)
        assert converter.convert(sparql_compact) == expected
