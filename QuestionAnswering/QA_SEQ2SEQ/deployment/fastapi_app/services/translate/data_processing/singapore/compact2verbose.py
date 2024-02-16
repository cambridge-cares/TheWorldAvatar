from services.translate.sparql import SparqlQuery
from services.translate.sparql.graph_pattern import (
    BindClause,
    GraphPattern,
    ServicePattern,
    TriplePattern,
)
from services.translate.sparql.solution_modifier import GroupClause, SolutionModifier
from services.translate.sparql.query_form import SelectClause
from services.translate.sparql.where_clause import WhereClause


class SgCompact2VerboseConverter:
    def __init__(self, ontop_endpoint: str):
        self.ontop_endpoint = ontop_endpoint

    def _try_converty_haslanduse_triple(self, pattern: GraphPattern):
        try:
            """
            ?Plot ozng:hasLandUseType/a ?LandUseTypeType .
            """
            assert isinstance(pattern, TriplePattern)
            assert len(pattern.tails) == 1
            subj = pattern.subj
            pred, obj = pattern.tails[0]
            assert pred == "ozng:hasLandUseType/a"

            """
            ?LandUseType a ?LandUseTypeType .
            SERVICE <?> {
                ?Plot ozng:hasLandUseType ?LandUseType .
            }
            """
            bg_patterns = [TriplePattern.from_triple("?LandUseType", "a", obj)]
            ontop_patterns = [
                TriplePattern.from_triple(subj, "ozng:hasLandUseType", "?LandUseType")
            ]
            return [], bg_patterns, ontop_patterns
        except AssertionError:
            return None

    def _try_convert_hasmeasure_triple(self, pattern: GraphPattern):
        try:
            """
            ?Plot *om:hasValue {key}.
            """
            assert isinstance(pattern, TriplePattern)
            assert len(pattern.tails) == 1
            pred, obj = pattern.tails[0]
            assert pred.endswith("om:hasValue")

            if not obj.startswith("?"):
                return [], [], [pattern]

            """
            ?Plot *om:hasValue {key} .
            ?{key} om:hasNumericalValue ?{key}NumericalValue ; 
                om:hasUnit/om:symbol ?{key}Unit .
            """
            numerical_value = obj + "NumericalValue"
            unit = obj + "Unit"
            vars = [numerical_value, unit]
            ontop_patterns = [
                pattern,
                TriplePattern(
                    obj,
                    tails=[
                        ("om:hasNumericalValue", numerical_value),
                        ("om:hasUnit", unit),
                    ],
                ),
            ]
            return vars, [], ontop_patterns
        except AssertionError:
            return None
        
    def _try_convert_isAwaitingDetailedGPREvaluation_triple(self, pattern: GraphPattern):
        try:
            assert isinstance(pattern, TriplePattern)
            assert len(pattern.tails) == 1
            pred, _ = pattern.tails[0]
            assert pred == "oplnrgl:isAwaitingDetailedGPREvaluation"
            return [], [], [pattern]
        except AssertionError:
            return None

    def convert(self, sparql_compact: SparqlQuery):
        select_vars_verbose = list(sparql_compact.select_clause.vars)

        bg_patterns_verbose, ontop_patterns_verbose = [], [TriplePattern.from_triple("?Plot", "a", "oplt:Plot")]
        for pattern in sparql_compact.where_clause.graph_patterns:
            flag = False
            for func in [
                self._try_converty_haslanduse_triple,
                self._try_convert_hasmeasure_triple,
                self._try_convert_isAwaitingDetailedGPREvaluation_triple
            ]:
                optional = func(pattern)
                if optional is not None:
                    vars, bg_patterns, ontop_patterns = optional
                    select_vars_verbose.extend(vars)
                    bg_patterns_verbose.extend(bg_patterns)
                    ontop_patterns_verbose.extend(ontop_patterns)
                    flag = True
                    break
            if not flag:
                bg_patterns_verbose.append(pattern)

        soln_mod = sparql_compact.solultion_modifier
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

        bg_patterns_verbose.append(
            ServicePattern(
                endpoint=self.ontop_endpoint, graph_patterns=ontop_patterns_verbose
            )
        )
        return SparqlQuery(
            select_clause=SelectClause(
                solution_modifier="DISTINCT", vars=select_vars_verbose
            ),
            where_clause=WhereClause(bg_patterns_verbose),
            solultion_modifier=soln_mod_verbose,
        )
