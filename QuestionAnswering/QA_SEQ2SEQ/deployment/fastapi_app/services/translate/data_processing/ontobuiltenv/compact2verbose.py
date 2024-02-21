from services.translate.sparql import SparqlQuery
from services.translate.sparql.graph_pattern import (
    BindClause,
    GraphPattern,
    OptionalClause,
    TriplePattern,
)
from services.translate.sparql.solution_modifier import GroupClause, SolutionModifier
from services.translate.sparql.query_form import SelectClause
from services.translate.sparql.where_clause import WhereClause


class OBECompact2VerboseConverter:
    def _expand_for_property(self):
        """
        ?Property obe:hasAddress ?Address .
        OPTIONAL {
            ?Address ict:hasBuilidingName ?BuildingName .
        }
        OPTIONAL {
            ?Address ict:hasStreet ?Street .
        }
        OPTIONAL {
            ?Address ict:hasStreetNumber ?StreetNumber .
        }
        OPTIONAL {
            ?Address obe:hasUnitName ?UnitName .
        }
        OPTIONAL {
            ?Address obe:hasPostalCode/rdfs:label ?PostalCode
        }
        """
        patterns = [
            TriplePattern.from_triple("?Property", "obe:hasAddress", "?Address"),
            OptionalClause(
                [
                    TriplePattern.from_triple(
                        "?Address", "ict:hasBuildingName", "?Building"
                    )
                ]
            ),
            OptionalClause(
                [TriplePattern.from_triple("?Address", "ict:hasStreet", "?Street")]
            ),
            OptionalClause(
                [
                    TriplePattern.from_triple(
                        "?Address", "ict:hasStreetNumber", "?StreetNumber"
                    )
                ]
            ),
            OptionalClause(
                [TriplePattern.from_triple("?Address", "obe:hasUnitName", "?UnitName")]
            ),
            OptionalClause(
                [
                    TriplePattern.from_triple(
                        "?Address", "obe:hasPostalCode/rdfs:label", "?PostalCode"
                    )
                ]
            ),
        ]
        vars = [
            "?Address",
            "?BuildingName",
            "?Street",
            "?StreetNumber",
            "?UnitName",
            "?PostalCode",
        ]

        return patterns, vars

    def _try_convert_property_hasmeasure_triple(self, pattern: GraphPattern):
        """
        ?Property obe:has{key} ?{key} .
        """
        if not (
            isinstance(pattern, TriplePattern)
            and pattern.subj == "?Property"
            and len(pattern.tails) == 1
        ):
            return None

        p, o = pattern.tails[0]

        key = p[len("obe:has") :]
        if not (
            key
            in [
                "TotalFloorArea",
                "MarketValue",
                "GroundElevation",
                "TotalMonetaryValue",
                "MonetaryValue",
            ]
            and o == "?" + key
        ):
            return None

        """
        ?Property obe:has{key} ?{key} .
        ?{key} om:hasValue [
            om:hasNumericalValue ?{key}NumericalValue ; 
            om:hasUnit/om:symbol ?{key}Unit
        ] .
        """
        patterns = [
            pattern,
            TriplePattern(
                "?{key}".format(key=key),
                tails=[
                    (
                        "om:hasValue",
                        "[ om:hasNumericalValue ?{key}NumericalValue ; om:hasUnit/om:symbol ?{key}Unit ]".format(
                            key=key
                        ),
                    )
                ],
            ),
        ]
        vars = [
            "?{key}NumericalValue".format(key=key),
            "?{key}Unit".format(key=key),
        ]
        return patterns, vars

    def _try_convert_property_hasaddress_triple(self, pattern: GraphPattern):
        """
        ?Property obe:hasAddress ?Address .
        """
        if not (
            isinstance(pattern, TriplePattern)
            and pattern.subj == "?Property"
            and len(pattern.tails) == 1
        ):
            return None

        p, o = pattern.tails[0]
        if not (p == "obe:hasAddress" and o == "?Address"):
            return None

        """
        ?Property obe:hasAddress ?Address .
        OPTIONAL {
            ?Address ict:hasBuilidingName ?BuildingName .
        }
        OPTIONAL {
            ?Address ict:hasStreet ?Street .
        }
        OPTIONAL {
            ?Address ict:hasStreetNumber ?StreetNumber .
        }
        OPTIONAL {
            ?Address obe:hasUnitName ?UnitName .
        }
        OPTIONAL {
            ?Address obe:hasPostalCode/rdfs:label ?PostalCode
        }
        """

        patterns = [
            pattern,
            OptionalClause(
                [
                    TriplePattern.from_triple(
                        "?Address", "ict:hasBuildingName", "?Building"
                    )
                ]
            ),
            OptionalClause(
                [TriplePattern.from_triple("?Address", "ict:hasStreet", "?Street")]
            ),
            OptionalClause(
                [
                    TriplePattern.from_triple(
                        "?Address", "ict:hasStreetNumber", "?StreetNumber"
                    )
                ]
            ),
            OptionalClause(
                [TriplePattern.from_triple("?Address", "obe:hasUnitName", "?UnitName")]
            ),
            OptionalClause(
                [
                    TriplePattern.from_triple(
                        "?Address", "obe:hasPostalCode/rdfs:label", "?PostalCode"
                    )
                ]
            ),
        ]
        vars = [
            "?BuildingName",
            "?Street",
            "?StreetNumber",
            "?UnitName",
            "?PostalCode",
        ]

        return patterns, vars

    def _try_convert_property_haspropertyusage_triple(self, pattern: GraphPattern):
        """
        ?Property obe:hasPropertyUsage ?PropertyUsage .
        """
        if not (
            isinstance(pattern, TriplePattern)
            and pattern.subj == "?Property"
            and len(pattern.tails) == 1
        ):
            return None

        p, o = pattern.tails[0]
        if not (p == "obe:hasPropertyUsage" and o == "?PropertyUsage"):
            return None

        """
        ?Property obe:hasPropertyUsage ?PropertyUsage .
        ?PropertyUsage a/rdfs:label ?PropertyUsageLabel .
        """
        patterns = [
            pattern,
            TriplePattern.from_triple(
                "?PropertyUsage", "a/rdfs:label", "?PropertyUsageLabel"
            ),
        ]
        vars = ["?PropertyUsageLabel"]
        return patterns, vars

    def convert(self, sparql_compact: SparqlQuery):
        select_vars_verbose = list(sparql_compact.select_clause.vars)

        if (
            "?Property" in sparql_compact.select_clause.vars
            and "?Address" not in sparql_compact.select_clause.vars
        ):
            graph_patterns_verbose, select_vars = self._expand_for_property()
            select_vars_verbose.extend(select_vars)
        else:
            graph_patterns_verbose = []

        for pattern in sparql_compact.where_clause.graph_patterns:
            optional = self._try_convert_property_hasmeasure_triple(pattern)
            if optional is not None:
                patterns, select_vars = optional
                select_vars_verbose.extend(select_vars)
                graph_patterns_verbose.extend(patterns)
                continue

            optional = self._try_convert_property_hasaddress_triple(pattern)
            if optional is not None:
                patterns, select_vars = optional
                select_vars_verbose.extend(select_vars)
                graph_patterns_verbose.extend(patterns)
                continue

            optional = self._try_convert_property_haspropertyusage_triple(pattern)
            if optional is not None:
                patterns, select_vars = optional
                select_vars_verbose.extend(select_vars)
                graph_patterns_verbose.extend(patterns)
                continue

            graph_patterns_verbose.append(pattern)

        for attrtype in ["?BuiltFormType", "?PropertyUsageType", "?PropertyTypeType"]:
            if attrtype in sparql_compact.select_clause.vars:
                attrtype_name = attrtype + "Name"
                select_vars_verbose.append(attrtype_name)
                graph_patterns_verbose.append(
                    BindClause(
                        exprn='REPLACE(STR({var}), "^.*/([^/]*)$", "$1")'.format(
                            var=attrtype
                        ),
                        var=attrtype_name,
                    )
                )

        soln_mod = sparql_compact.solution_modifier
        if soln_mod and soln_mod.group_clause:
            addn_vars = set(select_vars_verbose) - set(
                sparql_compact.select_clause.vars
            )
            soln_mod_verbose = SolutionModifier(
                group_clause=GroupClause(
                    vars=soln_mod.group_clause.vars + tuple(addn_vars)
                ),
                order_clause=soln_mod.order_clause,
                limit_clause=soln_mod.limit_clause,
            )
        else:
            soln_mod_verbose = soln_mod

        return SparqlQuery(
            select_clause=SelectClause(
                solution_modifier="DISTINCT", vars=select_vars_verbose
            ),
            where_clause=WhereClause(graph_patterns_verbose),
            solution_modifier=soln_mod_verbose,
        )
