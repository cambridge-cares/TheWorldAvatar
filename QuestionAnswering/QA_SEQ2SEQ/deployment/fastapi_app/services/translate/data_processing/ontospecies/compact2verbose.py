from typing import List

from services.translate.sparql import SparqlQuery
from services.translate.sparql.query_form import SelectClause
from services.translate.sparql.graph_pattern import (
    BindClause,
    GraphPattern,
    OptionalClause,
    TriplePattern,
    ValuesClause,
)
from services.translate.sparql.where_clause import WhereClause
from .constants import (
    IDENTIFIER_KEYS,
    PROPERTY_KEYS,
)


class OSSparqlCompact2VerboseConverter:
    def _try_convert_entity_linking_pattern(self, pattern: GraphPattern):
        """VALUES ?Species {{ {literals} }}"""
        if not (isinstance(pattern, ValuesClause) and pattern.var == "?Species"):
            return None

        """
        VALUES ?SpeciesIdentifierValue {{ {literals} }}
        ?Species ?hasIdentifier [ a/rdfs:subClassOf os:Identifier ; os:value ?SpeciesIdentifierValue ] .
        """
        patterns: List[GraphPattern] = [
            ValuesClause(var="?SpeciesIdentifierValue", values=pattern.values),
            TriplePattern.from_triple(
                "?Species",
                "?hasIdentifier",
                "[ a/rdfs:subClassOf os:Identifier ; os:value ?SpeciesIdentifierValue ]",
            ),
        ]
        select_vars: List[str] = []
        return patterns, select_vars

    def _make_species_hasproperty_patterns(self, key: str):
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
        patterns: List[GraphPattern] = [
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

    def _try_convert_species_haspropertyvalue_triple(self, pattern: GraphPattern):
        """?Species os:has{PropertyName}/os:value ?{PropertyName}Value ."""
        if not (
            isinstance(pattern, TriplePattern)
            and pattern.subj == "?Species"
            and len(pattern.tails) == 1
        ):
            return None

        predicate, obj = pattern.tails[0]
        if not (predicate.startswith("os:") and predicate.endswith("/os:value")):
            return None

        key = predicate[len("os:has") : -len("/os:value")]
        if not (
            key in PROPERTY_KEYS
            and obj == "?{PropertyName}Value".format(PropertyName=key)
        ):
            return None

        return self._make_species_hasproperty_patterns(key)

    def _try_convert_species_hasproperty_triple(self, pattern: GraphPattern):
        """?Species os:has{PropertyName} ?{PropertyName} ."""
        if not (
            isinstance(pattern, TriplePattern)
            and pattern.subj == "?Species"
            and len(pattern.tails) == 1
        ):
            return None

        predicate, obj = pattern.tails[0]
        if not predicate.startswith("os:"):
            return None

        key = predicate[len("os:has") :]
        if not key in PROPERTY_KEYS:
            return None
        # do not check for obj == "?" + key, because it could be that the predicated has been corrected

        return self._make_species_hasproperty_patterns(key)

    def _try_convert_species_haspropertyabstract_triple(self, pattern: GraphPattern):
        """?Species ?hasPropertyName ?PropertyName ."""
        if not (
            isinstance(pattern, TriplePattern)
            and pattern.subj == "?Species"
            and len(pattern.tails) == 1
        ):
            return None

        predicate, obj = pattern.tails[0]
        if not (predicate == "?hasPropertyName" and obj == "?PropertyName"):
            return None

        """
        ?Species ?hasPropertyName ?PropertyName .
        ?PropertyName os:value ?PropertyNameValue ; os:unit/rdfs:label ?PropertyNameUnitValue .
        OPTIONAL {
            ?PropertyName os:hasReferenceState [ os:value ?PropertyNameReferenceStateValue ; os:unit/rdfs:label ?PropertyNameReferenceStateUnitValue ] .
        }
        BIND(STRAFTER(STR(?hasPropertyName),'#has') AS ?PropertyLabel)
        """
        patterns: List[GraphPattern] = [
            pattern,
            TriplePattern(
                "?PropertyName",
                tails=[
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
            "?PropertyNameValue",
            "?PropertyNameUnitValue",
            "?PropertyNameReferenceStateValue",
            "?PropertyNameReferenceStateUnitValue",
        ]
        return patterns, select_vars

    def _try_convert_species_hasidentifier_triple(self, pattern: GraphPattern):
        """?Species os:has{IdentifierName} ?{IdentifierName} ."""
        if not (
            isinstance(pattern, TriplePattern)
            and pattern.subj == "?Species"
            and len(pattern.tails) == 1
        ):
            return None

        predicate, obj = pattern.tails[0]
        if not predicate.startswith("os:"):
            return None

        key = predicate[len("os:has") :]
        if not (key in IDENTIFIER_KEYS and obj == "?" + key):
            return None

        """
        ?Species os:has{IdentifierName} ?{IdentifierName} .
        ?{IdentifierName} os:value ?{IdentifierName}Value .
        """
        var_IdentifierNameValue = "?{IdentifierName}Value".format(IdentifierName=key)
        patterns = [
            pattern,
            TriplePattern.from_triple(
                "?" + key,
                "os:value",
                var_IdentifierNameValue,
            ),
        ]
        select_vars = [var_IdentifierNameValue]
        return patterns, select_vars

    def _try_convert_species_hasidentifierabstract_triple(self, pattern: GraphPattern):
        """?Species ?hasIdentifierName ?IdentifierName ."""
        if not (
            isinstance(pattern, TriplePattern)
            and pattern.subj == "?Species"
            and len(pattern.tails) == 1
        ):
            return None

        predicate, obj = pattern.tails[0]
        if not (predicate == "?hasIdentifierName" and obj == "?IdentifierName"):
            return None

        """
        ?Species ?hasIdentifierName ?IdentifierName .
        ?IdentifierName os:value ?IdentifierNameValue .
        BIND(STRAFTER(STR(?IdentifierName),'#') AS ?IdentifierLabel)
        """
        var_IdentifierNameValue = "?IdentifierNameValue"
        patterns = [
            pattern,
            TriplePattern.from_triple(
                "?IdentifierName",
                "os:value",
                var_IdentifierNameValue,
            ),
            BindClause(
                exprn="STRAFTER(STR(?hasIdentifierName),'#has')",
                var="?IdentifierLabel",
            ),
        ]
        select_vars = [var_IdentifierNameValue]
        return patterns, select_vars

    def _try_convert_species_haschemclasslabel_triple(self, pattern: GraphPattern):
        """?Species os:hasChemicalClass/rdfs:label ["chemclass"|?ChemicalClassLabel] ."""
        if not (
            isinstance(pattern, TriplePattern)
            and len(pattern.tails) == 1
            and pattern.subj == "?Species"
        ):
            return None

        predicate, obj = pattern.tails[0]
        if not (
            predicate == "os:hasChemicalClass/rdfs:label"
            and (
                (obj.startswith('"') and obj.endswith('"'))
                or obj == "?ChemicalClassLabel"
            )
        ):
            return None

        """
        ?Species (a|!a)+ [ rdf:type os:ChemicalClass ; rdfs:label {label} ] .
        """
        return TriplePattern.from_triple(
            "?Species",
            "(a|!a)+",
            "[ rdf:type os:ChemicalClass ; rdfs:label {label} ]".format(label=obj),
        )

    def convert(self, sparql_compact: SparqlQuery):
        graph_patterns = list(sparql_compact.where_clause.graph_patterns)
        graph_patterns.reverse()

        select_vars_verbose = list(sparql_compact.select_clause.vars)
        select_vars_verbose.append("?SpeciesLabel")
        graph_patterns_verbose = [
            TriplePattern(
                "?Species",
                tails=[("a", "os:Species"), ("rdfs:label", "?SpeciesLabel")],
            )
        ]

        while len(graph_patterns) > 0:
            pattern = graph_patterns.pop()

            optional = self._try_convert_entity_linking_pattern(pattern)
            if optional is not None:
                patterns, select_vars = optional
                select_vars_verbose.extend(select_vars)
                graph_patterns_verbose.extend(patterns)
                continue

            optional = self._try_convert_species_haspropertyvalue_triple(pattern)
            if optional is not None:
                patterns, select_vars = optional
                select_vars_verbose.extend(select_vars)
                graph_patterns_verbose.extend(patterns)
                continue

            optional = self._try_convert_species_hasproperty_triple(pattern)
            if optional is not None:
                patterns, select_vars = optional
                select_vars_verbose.extend(select_vars)
                graph_patterns_verbose.extend(patterns)
                continue

            optional = self._try_convert_species_haspropertyabstract_triple(pattern)
            if optional is not None:
                patterns, select_vars = optional
                select_vars_verbose.extend(select_vars)
                graph_patterns_verbose.extend(patterns)
                continue

            optional = self._try_convert_species_hasidentifier_triple(pattern)
            if optional is not None:
                patterns, select_vars = optional
                select_vars_verbose.extend(select_vars)
                graph_patterns_verbose.extend(patterns)
                continue

            optional = self._try_convert_species_hasidentifierabstract_triple(pattern)
            if optional is not None:
                patterns, select_vars = optional
                select_vars_verbose.extend(select_vars)
                graph_patterns_verbose.extend(patterns)
                continue

            optional = self._try_convert_species_haschemclasslabel_triple(pattern)
            if optional is not None:
                graph_patterns_verbose.append(optional)
                continue

            graph_patterns_verbose.append(pattern)

        return SparqlQuery(
            select_clause=SelectClause(
                solution_modifier="DISTINCT", vars=select_vars_verbose
            ),
            where_clause=WhereClause(graph_patterns_verbose),
            solution_modifier=sparql_compact.solution_modifier,
        )
