from typing import List, Tuple

from core.data_processing.constants import (
    ABSTRACT_IDENTIFIER_KEY,
    ABSTRACT_PROPERTY_KEY,
    CHEMCLASS_KEY,
    IDENTIFIER_KEYS,
    PROPERTY_KEYS,
    USE_KEY,
)
from core.data_processing.sparql import (
    BindClause,
    GraphPattern,
    OptionalClause,
    SelectClause,
    SparqlQuery,
    TriplePattern,
    ValuesClause,
)


class SparqlCompact2VerboseConverter:
    def _convert_values_clause(self, values_clause: ValuesClause) -> List[GraphPattern]:
        """VALUES ?Species {{ {literals} }}"""
        assert values_clause.var == "?Species", values_clause

        """
        VALUES ?SpeciesIdentifierValue {{ {literals} }}
        ?Species ?hasIdentifier [ rdf:type/rdfs:subClassOf os:Identifier ; os:value ?SpeciesIdentifierValue ] .
        """
        patterns = [
            ValuesClause(var="?SpeciesIdentifierValue", values=values_clause.values),
            TriplePattern.from_triple(
                "?Species",
                "?hasIdentifier",
                "[ rdf:type/rdfs:subClassOf os:Identifier ; os:value ?SpeciesIdentifierValue ]",
            ),
        ]
        select_vars = []
        return patterns, select_vars

    def _convert_triple_pattern_property_concrete(self, key: str):
        """
        ?Species os:has{PropertyName} ?{PropertyName} .
        ?{PropertyName} os:value ?{PropertyName}Value ; os:unit/rdfs:label ?{PropertyName}UnitLabel .
        OPTIONAL {{
            ?{PropertyName} os:hasReferenceState [ os:value ?{PropertyName}ReferenceStateValue ; os:unit/rdfs:label ?{PropertyName}ReferenceStateUnitLabel ] .
        }}
        """

        var_PropertyName = "?{PropertyName}".format(PropertyName=key)
        var_PropertyNameValue = "?{PropertyName}Value".format(PropertyName=key)
        var_PropertyNameUnitLabel = "?{PropertyName}UnitLabel".format(PropertyName=key)
        var_PropertyNameReferenceStateValue = (
            "?{PropertyName}ReferenceStateValue".format(PropertyName=key)
        )
        var_PropertyNameReferenceStateUnitLabel = (
            "?{PropertyName}ReferenceStateUnitLabel".format(PropertyName=key)
        )
        patterns = [
            TriplePattern.from_triple("?Species", "os:has" + key, var_PropertyName),
            TriplePattern(
                var_PropertyName,
                tails=[
                    (
                        "os:value",
                        var_PropertyNameValue,
                    ),
                    (
                        "os:unit/rdfs:label",
                        var_PropertyNameUnitLabel,
                    ),
                ],
            ),
            OptionalClause(
                [
                    TriplePattern.from_triple(
                        var_PropertyName,
                        "os:hasReferenceState",
                        "[ os:value ?{PropertyName}ReferenceStateValue ; os:unit/rdfs:label ?{PropertyName}ReferenceStateUnitLabel ]".format(
                            PropertyName=key
                        ),
                    )
                ]
            ),
        ]
        select_vars = [
            var_PropertyNameValue,
            var_PropertyNameUnitLabel,
            var_PropertyNameReferenceStateValue,
            var_PropertyNameReferenceStateUnitLabel,
        ]
        return patterns, select_vars

    def _convert_triple_pattern_identifier_concrete(self, key: str):
        """
        ?Species os:has{IdentifierName}/os:value ?{IdentifierName}Value .
        """
        var_IdentifierNameValue = "?{IdentifierName}Value".format(IdentifierName=key)
        patterns = [
            TriplePattern.from_triple(
                "?Species",
                "os:has{IdentifierName}/os:value".format(IdentifierName=key),
                var_IdentifierNameValue,
            )
        ]
        select_vars = [var_IdentifierNameValue]
        return patterns, select_vars

    def _convert_triple_pattern_use(self, use: str):
        """
        ?Species os:hasUse/rdfs:label {label} .
        """
        if use.startswith('"') and use.endswith('"'):
            select_vars = []
        elif use == "?" + USE_KEY:
            select_vars = ["?UseLabel"]
        patterns = [TriplePattern.from_triple("?Species", "os:hasUse/rdfs:label", use)]
        return patterns, select_vars

    def _convert_triple_pattern_chemclass(self, chemclass: str):
        """
        ?Species os:hasChemicalClass*/(rdf:|!rdf:)/rdfs:subClassOf* [ rdf:type os:ChemicalClass ; rdfs:label {label} ] .
        """
        if chemclass.startswith('"') and chemclass.endswith('"'):
            select_vars = []
        elif chemclass == "?" + CHEMCLASS_KEY:
            select_vars = ["?ChemicalClassLabel"]
        patterns = [
            TriplePattern.from_triple(
                "?Species",
                "os:hasChemicalClass*/(rdf:|!rdf:)/rdfs:subClassOf*",
                "[ rdf:type os:ChemicalClass ; rdfs:label {label} ]".format(
                    label=chemclass
                ),
            )
        ]
        return patterns, select_vars

    def _convert_triple_pattern_concrete_predicate(
        self, subj: str, predicate: str, obj: str
    ):
        assert subj == "?Species"
        assert predicate.startswith("os:")

        if predicate.endswith("/os:value"):
            key = predicate[len("os:has") : -len("/os:value")]
            if key in PROPERTY_KEYS:
                """?Species os:has{PropertyName}/os:value ?{PropertyName}Value"""
                assert predicate == "os:has{PropertyName}/os:value".format(
                    PropertyName=key
                ), predicate
                assert obj == "?{PropertyName}Value".format(PropertyName=key), obj
                patterns, select_vars = self._convert_triple_pattern_property_concrete(
                    key
                )
            else:
                raise ValueError("Unexpected predicate: " + predicate)
        elif predicate.endswith("/rdfs:label"):
            key = predicate[len("os:has") : -len("/rdfs:label")]
            if key == USE_KEY:
                patterns, select_vars = self._convert_triple_pattern_use(use=obj)
            elif key == CHEMCLASS_KEY:
                patterns, select_vars = self._convert_triple_pattern_chemclass(
                    chemclass=obj
                )
            else:
                raise ValueError("Unexpected predicate: " + predicate)
        else:
            key = predicate[len("os:has") :]
            assert obj == "?" + key, (predicate, obj)
            if key in PROPERTY_KEYS:
                patterns, select_vars = self._convert_triple_pattern_property_concrete(
                    key
                )
            elif key in IDENTIFIER_KEYS:
                (
                    patterns,
                    select_vars,
                ) = self._convert_triple_pattern_identifier_concrete(key)
            elif key == USE_KEY:
                patterns, select_vars = self._convert_triple_pattern_use(use=obj)
            elif key == CHEMCLASS_KEY:
                patterns, select_vars = self._convert_triple_pattern_chemclass(
                    chemclass=obj
                )
            else:
                raise ValueError("Unexpected predicate: " + predicate)

        return patterns, select_vars

    def _convert_triple_pattern_abstract_predicate(
        self, triple_link: Tuple[str, str, str], triple_subclassof: Tuple[str, str, str]
    ):
        """?Species ?has{key} ?{key} .
        ?{key} rdf:type/rdfs:subClassOf os:{keyClass} ."""
        subj_link, predicate_link, obj_link = triple_link
        assert subj_link == "?Species", subj_link
        assert predicate_link.startswith("?has"), predicate_link

        key = predicate_link[len("?has")]
        assert obj_link == "?{key}".format(key=key)

        subj_subclassof, predicate_subclassof, obj_subclassof = triple_subclassof
        assert subj_subclassof == obj_link
        assert predicate_subclassof == "rdf:type/rdfs:subClassOf"

        if key == ABSTRACT_PROPERTY_KEY:
            assert obj_subclassof == "os:Property"
            """
            ?Species ?hasPropertyName ?PropertyName .
            ?PropertyName rdf:type/rdfs:subClassOf os:Property ; os:value ?PropertyNameValue ; os:unit/rdfs:label ?PropertyNameUnitValue .
            OPTIONAL {
                ?PropertyName os:hasReferenceState [ os:value ?PropertyNameReferenceStateValue ; os:unit/rdfs:label ?PropertyNameReferenceStateUnitValue ] .
            }
            BIND(STRAFTER(STR(?hasPropertyName),'#has') AS ?PropertyLabel)
            """
            patterns = [
                TriplePattern.from_triple(
                    "?Species", "?hasPropertyName", "?PropertyName"
                ),
                TriplePattern(
                    "?PropertyName",
                    tails=[
                        ("rdf:type/rdfs:subClassOf", "os:Property"),
                        ("os:value", "?PropertyNameValue"),
                        ("os:unit/rdfs:label", "?PropertyNameUnitValue"),
                    ],
                ),
                OptionalClause(
                    [
                        TriplePattern.from_triple(
                            "?PropertyName",
                            "os:hasReferenceState",
                            "[ os:value ?PropertyNameReferenceStateValue ; os:unit/rdfs:label ?PropertyNameReferenceStateUnitValue ]",
                        )
                    ]
                ),
                BindClause(
                    exprn="STRAFTER(STR(?hasPropertyName),'#has')", var="?PropertyLabel"
                ),
            ]
            select_vars = [
                "?PropertyLabel",
                "?PropertyNameValue",
                "?PropertyNameUnitValue",
                "?PropertyNameReferenceStateValue",
                "?PropertyNameReferenceStateUnitValue",
            ]
        elif key == ABSTRACT_IDENTIFIER_KEY:
            assert obj_subclassof == "os:Identifier"
            """
            ?Species ?hasIdentifierName ?IdentifierName .
            ?IdentifierName rdf:type/rdfs:subClassOf os:Identifier ; os:value ?IdentifierNameValue .
            BIND(STRAFTER(STR(?IdentifierName),'#') AS ?IdentifierLabel)
            """
            patterns = [
                TriplePattern.from_triple(
                    "?Species", "?hasIdentifierName", "?IdentifierName"
                ),
                TriplePattern(
                    "?IdentifierName",
                    tails=[
                        "rdf:type/rdfs:subClassOf",
                        "os:Identifier",
                        "os:value",
                        "?IdentifierNameValue",
                    ],
                ),
                BindClause(
                    exprn="STRAFTER(STR(?hasIdentifierName),'#has')",
                    var="?IdentifierLabel",
                ),
            ]
            select_vars = ["?IdentifierLabel", "IdentifierNameValue"]
        else:
            raise ValueError("Unrecoginzed predicate: " + predicate_link)

        return patterns, select_vars

    def convert(self, sparql_compact: SparqlQuery):
        graph_patterns = list(sparql_compact.graph_patterns)

        select_vars_verbose = ["?SpeciesLabel"]
        graph_patterns_verbose = [
            TriplePattern(
                "?Species",
                tails=[("rdf:type", "os:Species"), ("rdfs:label", "?SpeciesLabel")],
            )
        ]

        while len(graph_patterns) > 0:
            pattern = graph_patterns.pop()
            if isinstance(pattern, ValuesClause):
                patterns, select_vars = self._convert_values_clause(pattern)
            elif isinstance(pattern, TriplePattern):
                assert len(pattern.tails) == 1, pattern

                subj = pattern.subj
                predicate, obj = pattern.tails[0]
                if not predicate.startswith("?"):
                    (
                        patterns,
                        select_vars,
                    ) = self._convert_triple_pattern_concrete_predicate(
                        subj, predicate, obj
                    )
                else:
                    pattern_subclassof = graph_patterns.pop()
                    assert isinstance(pattern_subclassof, TriplePattern)
                    assert len(pattern_subclassof.tails) == 1, pattern
                    (
                        patterns,
                        select_vars,
                    ) = self._convert_triple_pattern_abstract_predicate(
                        pattern, pattern_subclassof
                    )
            else:
                patterns = [pattern]
                select_vars = []

            select_vars_verbose.extend(select_vars)
            graph_patterns_verbose.extend(patterns)

        return SparqlQuery(
            select_clause=SelectClause(vars=select_vars_verbose),
            graph_patterns=graph_patterns_verbose,
        )
