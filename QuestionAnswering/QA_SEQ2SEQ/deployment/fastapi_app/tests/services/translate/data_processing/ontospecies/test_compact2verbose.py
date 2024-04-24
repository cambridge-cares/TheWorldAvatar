import pytest

from services.translate.data_processing.ontospecies.compact2verbose import (
    OSSparqlCompact2VerboseConverter,
)
from services.translate.sparql.graph_pattern import (
    FilterClause,
    OptionalClause,
    TriplePattern,
    ValuesClause,
)
from services.translate.sparql.where_clause import WhereClause
from services.translate.sparql.query_form import SelectClause
from services.translate.sparql import SparqlQuery


class TestOSCompact2VerboseConverter:
    @pytest.mark.parametrize(
        "sparql_compact,expected",
        [
            (
                # SELECT ?BoilingPoint ?InChI WHERE {
                #   VALUES ?Species { "ethanol" }
                #   ?Species os:hasBoilingPoint ?BoilingPoint .
                #   ?Species os:hasInChI ?InChI .
                # }
                SparqlQuery(
                    select_clause=SelectClause(vars=["?BoilingPoint", "?InChI"]),
                    where_clause=WhereClause(
                        [
                            ValuesClause(var="?Species", values=["ethanol"]),
                            TriplePattern.from_triple(
                                "?Species", "os:hasBoilingPoint", "?BoilingPoint"
                            ),
                            TriplePattern.from_triple(
                                "?Species", "os:hasInChI", "?InChI"
                            ),
                        ]
                    ),
                ),
                # SELECT DISTINCT ?BoilingPoint ?InChI ?SpeciesLabel ?BoilingPointValue ?BoilingPointUnitLabel ?BoilingPointReferenceStateValue ?BoilingPointReferenceStateUnitLabel ?InChIValue WHERE {
                #   ?Species a os:Species ; rdfs:label ?SpeciesLabel .
                #   VALUES ?SpeciesIdentifierValue { "ethanol" }
                #   ?Species ?hasIdentifier [ a/rdfs:subClassOf os:Identifier ; os:value ?SpeciesIdentifierValue ] .
                #   ?Species os:hasBoilingPoint ?BoilingPoint .
                #   ?BoilingPoint os:value ?BoilingPointValue ; os:unit/rdfs:label ?BoilingPointUnitLabel .
                #   OPTIONAL {
                #     ?BoilingPoint os:hasReferenceState [ os:value ?BoilingPointReferenceStateValue ; os:unit/rdfs:label ?BoilingPointReferenceStateUnitLabel ]
                #   }
                #   ?Species os:hasInChI ?InChI .
                #   ?InChi os:value ?InChIValue
                # }
                SparqlQuery(
                    select_clause=SelectClause(
                        vars=[
                            "?BoilingPoint",
                            "?InChI",
                            "?SpeciesLabel",
                            "?BoilingPointValue",
                            "?BoilingPointUnitLabel",
                            "?BoilingPointReferenceStateValue",
                            "?BoilingPointReferenceStateUnitLabel",
                            "?InChIValue",
                        ],
                        solution_modifier="DISTINCT",
                    ),
                    where_clause=WhereClause(
                        [
                            TriplePattern(
                                "?Species",
                                tails=[
                                    ("a", "os:Species"),
                                    ("rdfs:label", "?SpeciesLabel"),
                                ],
                            ),
                            ValuesClause(
                                var="?SpeciesIdentifierValue", values=["ethanol"]
                            ),
                            TriplePattern.from_triple(
                                "?Species",
                                "?hasIdentifier",
                                "[ a/rdfs:subClassOf os:Identifier ; os:value ?SpeciesIdentifierValue ]",
                            ),
                            TriplePattern.from_triple(
                                "?Species", "os:hasBoilingPoint", "?BoilingPoint"
                            ),
                            TriplePattern(
                                "?BoilingPoint",
                                tails=[
                                    ("os:value", "?BoilingPointValue"),
                                    ("os:unit/rdfs:label", "?BoilingPointUnitLabel"),
                                ],
                            ),
                            OptionalClause(
                                graph_patterns=[
                                    TriplePattern.from_triple(
                                        "?BoilingPoint",
                                        "os:hasReferenceState",
                                        "[ os:value ?BoilingPointReferenceStateValue ; os:unit/rdfs:label ?BoilingPointReferenceStateUnitLabel ]",
                                    )
                                ]
                            ),
                            TriplePattern.from_triple(
                                "?Species", "os:hasInChI", "?InChI"
                            ),
                            TriplePattern.from_triple(
                                "?InChI", "os:value", "?InChIValue"
                            ),
                        ]
                    ),
                ),
            ),
            (
                # SELECT ?Species WHERE {
                #   ?Species os:hasMeltingPoint/os:value ?MeltingPointValue .
                #   FILTER ( ?MeltingPointValue > 100 )
                # }
                SparqlQuery(
                    select_clause=SelectClause(vars=["?Species"]),
                    where_clause=WhereClause(
                        [
                            TriplePattern.from_triple(
                                "?Species",
                                "os:hasMeltingPoint/os:value",
                                "?MeltingPointValue",
                            ),
                            FilterClause("?MeltingPointValue > 100"),
                        ]
                    ),
                ),
                # SELECT ?Species ?SpeciesLabel ?MeltingPointValue ?MeltingPointUnitLabel ?MeltingPointReferenceStateValue ?MeltingPointReferenceStateUnitLabel WHERE {
                #   ?Species a os:Species ; rdfs:label ?SpeciesLabel .
                #   ?Species os:hasMeltingPoint ?MeltingPoint .
                #   ?MeltingPoint os:value ?MeltingPointValue ; os:unit/rdfs:label ?MeltingPointUnitLabel .
                #   OPTIONAL {
                #     ?MeltingPoint os:hasReferenceState [ os:value ?MeltingPointReferenceStateValue ; os:unit/rdfs:label ?MeltingPointReferenceStateUnitLabel ]
                #   }
                #   FILTER ( ?MeltingPointValue > 100 )
                # }
                SparqlQuery(
                    select_clause=SelectClause(
                        vars=[
                            "?Species",
                            "?SpeciesLabel",
                            "?MeltingPointValue",
                            "?MeltingPointUnitLabel",
                            "?MeltingPointReferenceStateValue",
                            "?MeltingPointReferenceStateUnitLabel",
                        ],
                        solution_modifier="DISTINCT",
                    ),
                    where_clause=WhereClause(
                        [
                            TriplePattern(
                                "?Species",
                                tails=[
                                    ("a", "os:Species"),
                                    ("rdfs:label", "?SpeciesLabel"),
                                ],
                            ),
                            TriplePattern.from_triple(
                                "?Species", "os:hasMeltingPoint", "?MeltingPoint"
                            ),
                            TriplePattern(
                                "?MeltingPoint",
                                tails=[
                                    ("os:value", "?MeltingPointValue"),
                                    ("os:unit/rdfs:label", "?MeltingPointUnitLabel"),
                                ],
                            ),
                            OptionalClause(
                                graph_patterns=[
                                    TriplePattern.from_triple(
                                        "?MeltingPoint",
                                        "os:hasReferenceState",
                                        "[ os:value ?MeltingPointReferenceStateValue ; os:unit/rdfs:label ?MeltingPointReferenceStateUnitLabel ]",
                                    )
                                ]
                            ),
                            FilterClause("?MeltingPointValue > 100"),
                        ]
                    ),
                ),
            ),
            (
                # SELECT ?UseLabel ? WHERE {
                #   ?Species os:hasChemicalClass/rdfs:label "alcohol" .
                #   ?Species os:hasUse/rdfs:label ?UseLabel .
                # }
                SparqlQuery(
                    select_clause=SelectClause(vars=["?UseLabel"]),
                    where_clause=WhereClause(
                        [
                            TriplePattern.from_triple(
                                "?Species",
                                "os:hasChemicalClass/rdfs:label",
                                '"alcohol"',
                            ),
                            TriplePattern.from_triple(
                                "?Species", "os:hasUse/rdfs:label", "?UseLabel"
                            ),
                        ]
                    ),
                ),
                # SELECT ?UseLabel ?SpeciesLabel WHERE {
                #   ?Species a os:Species ; rdfs:label ?SpeciesLabel .
                #   ?Species (a|!a)+ [ rdf:type os:ChemicalClass ; rdfs:label "alcohol" ] .
                #   ?Species os:hasUse/rdfs:label ?UseLabel
                # }
                SparqlQuery(
                    select_clause=SelectClause(
                        vars=["?UseLabel", "?SpeciesLabel"],
                        solution_modifier="DISTINCT",
                    ),
                    where_clause=WhereClause(
                        [
                            TriplePattern(
                                "?Species",
                                tails=[
                                    ("a", "os:Species"),
                                    ("rdfs:label", "?SpeciesLabel"),
                                ],
                            ),
                            TriplePattern.from_triple(
                                "?Species",
                                "(a|!a)+",
                                '[ rdf:type os:ChemicalClass ; rdfs:label "alcohol" ]',
                            ),
                            TriplePattern.from_triple(
                                "?Species", "os:hasUse/rdfs:label", "?UseLabel"
                            ),
                        ]
                    ),
                ),
            ),
        ],
    )
    def test_convert(self, sparql_compact, expected):
        converter = OSSparqlCompact2VerboseConverter()
        assert converter.convert(sparql_compact) == expected
