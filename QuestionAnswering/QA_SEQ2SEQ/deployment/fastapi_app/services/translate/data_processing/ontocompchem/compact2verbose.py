from services.translate.sparql import SparqlQuery
from services.translate.sparql.query_form import SelectClause
from services.translate.sparql.graph_pattern import (
    GraphPattern,
    TriplePattern,
)
from services.translate.sparql.where_clause import WhereClause


class OCCSparqlCompact2VerboseConverter:
    def _try_convert_molcomp_hasresult_triple(self, pattern: GraphPattern):
        """?MolecularComputation occ:hasResult ?{ResultKey} ."""
        if not (
            isinstance(pattern, TriplePattern)
            and pattern.subj == "?MolecularComputation"
            and len(pattern.tails) == 1
        ):
            return None

        pred, result_var = pattern.tails[0]
        if not (pred == "occ:hasResult" and result_var[0] == "?"):
            return None

        result_key = result_var[1:]
        if result_key == "OptimizedGeometry":
            """
            ?MolecularComputation occ:hasResult ?OptimizedGeometry .
            ?OptimizedGeometry ^os:fromGeometry ?X, ?Y, ?Z  .
            ?Atom os:hasXCoordinate ?X ;
                    os:hasYCoordinate ?Y ;
                    os:hasZCoordinate ?Z .
            ?X os:value ?XValue ; os:unit ?XUnit .
            ?Y os:value ?YValue ; os:unit ?YUnit .
            ?Z os:value ?ZValue ; os:unit ?ZUnit .
            """
            patterns = [
                pattern,
                TriplePattern(
                    subj="?X",
                    tails=[
                        ("os:fromGeometry", "?OptimizedGeometry"),
                        ("os:value", "?XValue"),
                        ("os:unit", "?XUnit"),
                    ],
                ),
                TriplePattern(
                    subj="?Y",
                    tails=[
                        ("os:fromGeometry", "?OptimizedGeometry"),
                        ("os:value", "?YValue"),
                        ("os:unit", "?YUnit"),
                    ],
                ),
                TriplePattern(
                    subj="?Z",
                    tails=[
                        ("os:fromGeometry", "?OptimizedGeometry"),
                        ("os:value", "?ZValue"),
                        ("os:unit", "?ZUnit"),
                    ],
                ),
                TriplePattern(
                    subj="?Atom",
                    tails=[
                        ("os:hasXCoordinate", "?X"),
                        ("os:hasYCoordinate", "?Y"),
                        ("os:hasZCoordinate", "?Z"),
                    ],
                ),
            ]
            select_vars = [
                "?Atom",
                "?X",
                "?XValue",
                "?XUnit",
                "?Y",
                "?YValue",
                "?YUnit",
                "?Z",
                "?ZValue",
                "?ZUnit",
            ]
        elif result_key == "RotationalConstants":
            """
            ?MolecularComputation occ:hasResult ?RotationalConstants .
            ?RotationalConstants occ:value ?RotationalConstantsValue .
            """
            patterns = [
                pattern,
                TriplePattern.from_triple(
                    "?RotationalConstants", "occ:value", "?RotationalConstantsValue"
                ),
            ]
            select_vars = ["?RotationalConstantsValue"]
        elif result_key in [
            "SCFEnergy",
            "TotalGibbsFreeEnergy",
            "ZeroPointEnergy",
            "HOMOEnergy",
            "HOMOMinus1Energy",
            "HOMOMinus2Energy",
            "LUMOEnergy",
            "LUMOPlus1Energy",
            "LUMOPlus2Energy",
            "TotalEnergy",
            "TotalEnthalpy",
            "Frequencies",
        ]:
            """
            ?MolecularComputation occ:hasResult ?{ResultKey} .
            ?{ResultKey} occ:value ?{ResultKey}Value ; occ:unit ?{ResultKey}Unit .
            """
            result_value_var = result_var + "Value"
            result_unit_var = result_var + "Unit"
            patterns = [
                pattern,
                TriplePattern(
                    subj=result_var,
                    tails=[
                        ("occ:value", result_value_var),
                        ("occ:unit", result_unit_var),
                    ],
                ),
            ]
            select_vars = [result_value_var, result_unit_var]
        else:
            return None
        return patterns, select_vars

    def convert(self, sparql_compact: SparqlQuery):
        select_vars_verbose = []
        if "?BasisSetLabel" not in sparql_compact.select_clause.vars:
            select_vars_verbose.append("?BasisSetLabel")
        if "?LevelOfTheoryLabel" not in sparql_compact.select_clause.vars:
            select_vars_verbose.append("?LevelOfTheoryLabel")
        select_vars_verbose += list(sparql_compact.select_clause.vars)

        graph_patterns_verbose = []
        basisset_pattern = TriplePattern.from_triple(
            "?MolecularComputation",
            "occ:hasMethodology/occ:hasBasisSet/rdfs:label",
            "?BasisSetLabel",
        )
        if basisset_pattern not in sparql_compact.where_clause.graph_patterns:
            graph_patterns_verbose.append(basisset_pattern)

        leveloftheory_pattern = TriplePattern.from_triple(
            "?MolecularComputation",
            "occ:hasMethodology/occ:hasLevelOfTheory/rdfs:label",
            "?LevelOfTheoryLabel",
        )
        if leveloftheory_pattern not in sparql_compact.where_clause.graph_patterns:
            graph_patterns_verbose.append(leveloftheory_pattern)

        for pattern in reversed(sparql_compact.where_clause.graph_patterns):
            optional = self._try_convert_molcomp_hasresult_triple(pattern)
            if optional is not None:
                patterns, select_vars = optional
                select_vars_verbose.extend(select_vars)
                graph_patterns_verbose.extend(patterns)
                continue

            graph_patterns_verbose.append(pattern)

        return SparqlQuery(
            select_clause=SelectClause(
                solution_modifier="DISTINCT", vars=select_vars_verbose
            ),
            where_clause=WhereClause(graph_patterns_verbose),
            solution_modifier=sparql_compact.solution_modifier,
        )
