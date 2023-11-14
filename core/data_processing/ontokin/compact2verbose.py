from core.sparql import SparqlQuery
from core.sparql.query_form import SelectClause
from core.sparql.graph_pattern import (
    GraphPattern,
    TriplePattern,
)


class OKSparqlCompact2VerboseConverter:
    def _try_convert_species_hasthermomodel_triple(self, pattern: GraphPattern):
        try:
            """?Species okin:hasThermoModel ?ThermoModel ."""
            assert isinstance(pattern, TriplePattern)
            assert pattern.subj == "?Species"
            assert len(pattern.tails) == 1

            predicate, obj = pattern.tails[0]
            assert predicate == "okin:hasThermoModel"
            assert obj == "?ThermoModel"

            """
            ?Species okin:hasThermoModel ?ThermoModel .
            ?ThermoModel okin:hasCoefficientValues ?CoefficientValues ;
                         okin:hasNumberOfCoefficients ?NumberOfCoefficients ;
                         okin:hasMaximumTemperature ?MaximumTemperature ;
                         okin:hasMinimumTemperature ?MinimumTemperature ;
                         okin:hasPressure ?Pressure .
            """
            patterns = [
                pattern,
                TriplePattern(
                    subj="?ThermoModel",
                    tails=[
                        ("okin:hasCoefficientValues", "?CoefficientValues"),
                        ("okin:hasNumberOfCoefficients", "?NumberOfCoefficients"),
                        ("okin:hasMaximumTemperature", "?MaximumTemperature"),
                        ("okin:hasMinimumTemperature", "?MinimumTemperature"),
                        ("okin:hasPressure", "?Pressure"),
                    ],
                ),
            ]
            select_vars = [
                "?CoefficientValues",
                "?NumberOfCoefficients",
                "?MaximumTemperature",
                "?MinimumTemperature",
                "?Pressure",
            ]
            return patterns, select_vars
        except AssertionError:
            return None

    def _try_convert_species_hastransportmodel_triple(self, pattern: GraphPattern):
        try:
            """?Species okin:hasTransportModel ?TransportModel ."""
            assert isinstance(pattern, TriplePattern)
            assert pattern.subj == "?Species"
            assert len(pattern.tails) == 1

            predicate, obj = pattern.tails[0]
            assert predicate == "okin:hasTransportModel"
            assert obj == "?TransportModel"

            """
            ?Species okin:hasTransportModel ?TransportModel .
            ?TransportModel okin:hasDipoleMoment ?DipoleMoment ;
                            okin:hasDipoleMomentUnits ?DipoleMomentUnits ;
                            okin:hasLennardJonesDiameter ?LennardJonesDiameter ;
                            okin:hasLennardJonesDiameterUnits ?LennardJonesDiameterUnits ;
                            okin:hasLennardJonesWellDepth ?LennardJonesWellDepth ;
                            okin:hasLennardJonesWellDepthUnits ?LennardJonesWellDepthUnits ;
                            okin:hasPolarizability ?Polarizability ;
                            okin:hasPolarizabilityUnits ?PolarizabilityUnits ;
                            okin:hasRotationalRelaxationCollisionNumber ?RotationalRelaxationCollisionNumber ;
                            okin:hasRotationalRelaxationCollisionNumberUnits ?RotationalRelaxationCollisionNumberUnits ;
                            okin:hasSpeciesGeometry ?SpeciesGeometry ;
                            okin:hasSpeciesGeometryTitle ?SpeciesGeometryTitle .
            """
            patterns = [
                pattern,
                TriplePattern(
                    subj="?TransportModel",
                    tails=[
                        ("okin:hasDipoleMoment", "?DipoleMoment"),
                        ("okin:hasDipoleMomentUnits", "?DipoleMomentUnits"),
                        ("okin:hasLennardJonesDiameter", "?LennardJonesDiameter"),
                        (
                            "okin:hasLennardJonesDiameterUnits",
                            "?LennardJonesDiameterUnits",
                        ),
                        ("okin:hasLennardJonesWellDepth", "?LennardJonesWellDepth"),
                        (
                            "okin:hasLennardJonesWellDepthUnits",
                            "?LennardJonesWellDepthUnits",
                        ),
                        ("okin:hasPolarizability", "?Polarizability"),
                        ("okin:hasPolarizabilityUnits", "?PolarizabilityUnits"),
                        (
                            "okin:hasRotationalRelaxationCollisionNumber",
                            "?RotationalRelaxationCollisionNumber",
                        ),
                        (
                            "okin:hasRotationalRelaxationCollisionNumberUnits",
                            "?RotationalRelaxationCollisionNumberUnits",
                        ),
                        ("okin:hasSpeciesGeometry", "?SpeciesGeometry"),
                        ("okin:hasSpeciesGeometryTitle", "?SpeciesGeometryTitle"),
                    ],
                ),
            ]
            select_vars = [
                "?DipoleMoment",
                "?DipoleMomentUnits",
                "?LennardJonesDiameter",
                "?LennardJonesDiameterUnits",
                "?LennardJonesWellDepth",
                "?LennardJonesWellDepthUnits",
                "?Polarizability",
                "?PolarizabilityUnits",
                "?RotationalRelaxationCollisionNumber",
                "?RotationalRelaxationCollisionNumberUnits",
                "?SpeciesGeometry",
                "?SpeciesGeometryTitle",
            ]
            return patterns, select_vars
        except AssertionError:
            return None

    def _try_convert_rxn_hasratecoeffs_triple(self, pattern: GraphPattern):
        pass

    def convert(self, sparql_compact: SparqlQuery):
        graph_patterns = list(sparql_compact.graph_patterns)
        graph_patterns.reverse()

        select_vars_verbose = list(sparql_compact.select_clause.vars)
        graph_patterns_verbose = []

        while len(graph_patterns) > 0:
            pattern = graph_patterns.pop()

            optional = self._try_convert_species_hasthermomodel_triple(pattern)
            if optional is not None:
                patterns, select_vars = optional
                select_vars_verbose.extend(select_vars)
                graph_patterns_verbose.extend(patterns)
                continue

            optional = self._try_convert_species_hastransportmodel_triple(pattern)
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
            graph_patterns=graph_patterns_verbose,
        )
