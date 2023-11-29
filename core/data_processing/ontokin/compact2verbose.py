from typing import List, Literal, Optional, Set
from core.sparql import SparqlQuery
from core.sparql.query_form import SelectClause
from core.sparql.graph_pattern import (
    BindClause,
    GraphPattern,
    OptionalClause,
    TriplePattern,
    ValuesClause,
)


class OKSparqlCompact2VerboseConverter:
    def _get_types_topicentities_selectvars(self, query: SparqlQuery):
        vars: Set[str] = set()

        for var in query.select_clause.vars:
            if var in ["?Species", "?ThermoModel", "TransportModel"]:
                vars.add("Species")
            elif var in ["?Reaction", "?KineticModel"]:
                vars.add("Reaction")
            elif var == "?Mechanism":
                vars.add("Mechanism")

        return tuple(vars)

    def _is_entity_grounded(
        self,
        query: SparqlQuery,
        topic_entity_type: Optional[Literal["Species", "Reaction", "Mechanism"]],
    ):
        if topic_entity_type is None:
            return None

        def is_species_grounded(pattern: GraphPattern):
            return (
                isinstance(pattern, TriplePattern)
                and pattern.subj == "?Species"
                and any(pred == "skos:altLabel" for pred, _ in pattern.tails)
            )

        def is_rxn_grounded(pattern: GraphPattern):
            return (
                isinstance(pattern, TriplePattern)
                and pattern.subj == "?Reaction"
                and any(pred == "okin:hasEquation" for pred, _ in pattern.tails)
            )

        def is_mechanism_grounded(pattern: GraphPattern):
            return (
                isinstance(pattern, TriplePattern)
                and pattern.subj == "?Mechanism"
                and any(pred == "okin:hasProvenance/op:hasDOI" for pred, _ in pattern.tails)
            )

        if topic_entity_type == "Species":
            checker = is_species_grounded
        elif topic_entity_type == "Reaction":
            checker = is_rxn_grounded
        elif topic_entity_type == "Mechanism":
            checker = is_mechanism_grounded
        else:
            checker = lambda _: False

        return any(checker(pattern) for pattern in query.graph_patterns)

    def add_type_and_label_for_select_entities(self, query: SparqlQuery):
        patterns = []
        select_vars = []

        entity_types = self._get_types_topicentities_selectvars(query)
        for entity_type in entity_types:
            is_entity_grounded = self._is_entity_grounded(
                query, entity_types
            )
            if entity_type == "Species":
                if is_entity_grounded:
                    patterns.append(
                        TriplePattern.from_triple(
                            "?Species", "a/rdfs:subClassOf*", "os:Species"
                        )
                    )
                else:
                    patterns.append(
                        TriplePattern(
                            subj="?Species",
                            tails=[
                                ("a/rdfs:subClassOf*", "os:Species"),
                                ("skos:altLabel", "?SpeciesLabel"),
                            ],
                        )
                    )
                    select_vars.append("?SpeciesLabel")
            elif entity_type == "Reaction":
                if is_entity_grounded:
                    patterns.append(
                        TriplePattern.from_triple("?Reaction", "a", "okin:GasPhaseReaction")
                    )
                else:
                    patterns.append(
                        TriplePattern(
                            subj="?Reaction",
                            tails=[
                                ("a", "okin:GasPhaseReaction"),
                                ("okin:hasEquation", "?ReactionEquation"),
                            ],
                        )
                    )
                    select_vars.append("?ReactionEquation")
            elif entity_type == "Mechanism":
                if is_entity_grounded:
                    patterns.append(
                        TriplePattern.from_triple(
                            "?Mechanism", "a", "okin:ReactionMechanism"
                        )
                    )
                else:
                    patterns.append(
                        TriplePattern(
                            subj="?Mechanism",
                            tails=[
                                ("a", "okin:ReactionMechanism"),
                                ("okin:hasProvenance/op:hasDOI", "?DOI"),
                            ],
                        )
                    )
                    select_vars.append("?DOI")

        return patterns, select_vars

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
            ?ThermoModel okin:hasPolynomial [
                            okin:hasA1 ?A1 ;
                            okin:hasA2 ?A2 ;
                            okin:hasA3 ?A3 ;
                            okin:hasA4 ?A4 ;
                            okin:hasA5 ?A5 ;
                            okin:hasA6 ?A6 ;
                            okin:hasA7 ?A7 ;
                            okin:hasB1 ?B1 ;
                            okin:hasB2 ?B2 ;
                            okin:hasTmin [ okin:value ?PolyTminValue ; okin:unit ?PolyTminUnit ] ;
                            okin:hasTmax [ okin:value ?PolyTmaxValue ; okin:unit ?PolyTmaxUnit ]
                         ] ;
                         okin:hasTmin [ okin:value ?TminValue ; okin:unit ?TminUnit ] ;
                         okin:hasTmax [ okin:value ?TmaxValue ; okin:unit ?TmaxUnit ] .
            """
            patterns = [
                pattern,
                TriplePattern(
                    subj="?ThermoModel",
                    tails=[
                        (
                            "okin:hasPolynomial",
                            "[ okin:hasA1 ?A1 ; okin:hasA2 ?A2 ; okin:hasA3 ?A3 ; okin:hasA4 ?A4 ; okin:hasA5 ?A5 ; okin:hasA6 ?A6 ; okin:hasA7 ?A7 ; okin:hasB1 ?B1 ; okin:hasB2 ?B2 ; okin:hasTmin [ okin:value ?PolyTminValue ; okin:unit ?PolyTminUnit ] ; okin:hasTmax [ okin:value ?PolyTmaxValue ; okin:unit ?PolyTmaxUnit ] ]",
                        ),
                        (
                            "okin:hasTmin",
                            "[ okin:value ?TminValue ; okin:unit ?TminUnit ]",
                        ),
                        (
                            "okin:hasTmax",
                            "[ okin:value ?TmaxValue ; okin:unit ?TmaxUnit ]",
                        ),
                    ],
                ),
            ]
            select_vars = [
                "?A1",
                "?A2",
                "?A3",
                "?A4",
                "?A5",
                "?A6",
                "?A7",
                "?B1",
                "?B2",
                "?PolyTminValue",
                "?PolyTminUnit",
                "?PolyTmaxValue",
                "?PolyTmaxUnit",
                "?TminValue",
                "?TminUnit",
                "?TmaxValue",
                "?TmaxUnit",
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
            ?TransportModel okin:hasDipoleMoment [ okin:value ?DipoleMomentValue ; okin:unit ?DipoleMomentUnit ] ;
                            okin:hasLJCollisionDiameter [ okin:value ?LJColissionDiameterValue ; okin:unit ?LJColissionDiameterUnit ] ;
                            okin:hasLJPotentialWellDepth [ okin:value ?LJPotentialWellDepthValue ; okin:unit ?LJPotentialWellDepthUnit ] ;
                            okin:hasPolarizability [ okin:value ?PolarizabilityValue ; okin:unit ?PolarizabilityUnit ] ;
                            okin:hasRotationalRelaxationCollisionNumber/okin:value ?RotationalRelaxationCollisionNumberValue ;
                            okin:hasShapeIndex/okin:value ?ShapeIndexValue .
            """
            patterns = [
                pattern,
                TriplePattern(
                    subj="?TransportModel",
                    tails=[
                        (
                            "okin:hasDipoleMoment",
                            "[ okin:value ?DipoleMomentValue ; okin:unit ?DipoleMomentUnit ]",
                        ),
                        (
                            "okin:hasLJCollisionDiameter",
                            "[ okin:value ?LJColissionDiameterValue ; okin:unit ?LJColissionDiameterUnit ]",
                        ),
                        (
                            "okin:hasLJPotentialWellDepth",
                            "[ okin:value ?LJPotentialWellDepthValue ; okin:unit ?LJPotentialWellDepthUnit ]",
                        ),
                        (
                            "okin:hasPolarizability",
                            "[ okin:value ?PolarizabilityValue ; okin:unit ?PolarizabilityUnit ]",
                        ),
                        (
                            "okin:hasRotationalRelaxationCollisionNumber/okin:value",
                            "?RotationalRelaxationCollisionNumberValue",
                        ),
                        ("okin:hasShapeIndex/okin:value", "?ShapeIndexValue"),
                    ],
                ),
            ]
            select_vars = [
                "?DipoleMomentValue",
                "?DipoleMomentUnit",
                "?LJColissionDiameterValue",
                "?LJColissionDiameterUnit",
                "?LJPotentialWellDepthValue",
                "?LJPotentialWellDepthUnit",
                "?PolarizabilityValue",
                "?PolarizabilityUnit",
                "?RotationalRelaxationCollisionNumberValue",
                "?ShapeIndexValue",
            ]
            return patterns, select_vars
        except AssertionError:
            return None

    def _try_convert_rxn_haskineticmodel_triple(self, pattern: GraphPattern):
        try:
            """?Reaction okin:hasKineticModel ?KineticModel ."""
            assert isinstance(pattern, TriplePattern)
            assert pattern.subj == "?Reaction"
            assert len(pattern.tails) == 1

            predicate, obj = pattern.tails[0]
            assert predicate == "okin:hasKineticModel"
            assert obj == "?KineticModel"

            """
            ?Reaction okin:hasKineticModel ?KineticModel .
            ?KineticModel a ?KineticModelType .
            BIND (STRAFTER(STR(?KineticModelType), "#") AS ?ModelType)
            OPTIONAL {
                VALUES ?KineticModelType { okin:ArrheniusModel }
                ?KineticModel okin:hasActivationEnergy [ okin:value ?ActivationEnergyValue ; okin:unit ?ActivationEnergyUnit ] ;
                              okin:hasArrheniusFactor [ okin:value ?ArrheniusFactorValue ; okin:unit ?ArrheniusFactorUnit ] ;
                              okin:hasTemperatureExponent/okin:value ?TemperatureExponentValue .
            }
            OPTIONAL {
                VALUES ?KineticModelType { okin:MultiArrheniusModel }
                ?KineticModel okin:hasArrheniusModel ?ArrheniusModel .
                ?ArrheniusModel okin:hasActivationEnergy [ okin:value ?ActivationEnergyValue ; okin:unit ?ActivationEnergyUnit ] ;
                                okin:hasArrheniusFactor [ okin:value ?ArrheniusFactorValue ; okin:unit ?ArrheniusFactorUnit ] ;
                                okin:hasTemperatureExponent/okin:value ?TemperatureExponentValue .
            }
            OPTIONAL {
                VALUES ?KineticModelType { okin:ThreeBodyReactionModel okin:LindemannModel okin:TroeModel }
                OPTIONAL { 
                    ?KineticModel okin:hasCollider [ rdfs:label ?ColliderLabel ; okin:hasEfficiency ?ColliderEfficiency ] .
                }
                ?KineticModel okin:hasArrheniusLowModel ?ArrheniusLowModel .
                ?ArrheniusLowModel okin:hasActivationEnergy [ okin:value ?ActivationEnergyLowValue ; okin:unit ?ActivationEnergyLowUnit ] ;
                                   okin:hasArrheniusFactor [ okin:value ?ArrheniusFactorLowValue ; okin:unit ?ArrheniusFactorLowUnit ] ;
                                   okin:hasTemperatureExponent/okin:value ?TemperatureExponentLowValue .
                OPTIONAL {
                    ?KineticModel okin:hasArrheniusHighModel ?ArrheniusHighModel ;
                    ?ArrheniusHighModel okin:hasActivationEnergy [ okin:value ?ActivationEnergyHighValue ; okin:unit ?ActivationEnergyHighUnit ] ;
                                        okin:hasArrheniusFactor [ okin:value ?ArrheniusFactorHighValue ; okin:unit ?ArrheniusFactorHighUnit ] ;
                                        okin:hasTemperatureExponent/okin:value ?TemperatureExponentHighValue .
                    OPTIONAL {
                        ?KineticModel okin:hasAlpha/okin:value ?AlphaValue ;
                                      okin:hasT1/okin:value ?T1Value ;
                                      okin:hasT2/okin:value ?T2Value ;
                                      okin:hasT3/okin:value ?T3Value .
                    }
                }
            }
            """
            arrheniusmodel_patterns = [
                ValuesClause(var="?KineticModelType", values=["okin:ArrheniusModel"]),
                TriplePattern(
                    subj="?KineticModel",
                    tails=[
                        ("a", "okin:ArrheniusModel"),
                        (
                            "okin:hasActivationEnergy",
                            "[ okin:value ?ActivationEnergyValue ; okin:unit ?ActivationEnergyUnit ]",
                        ),
                        (
                            "okin:hasArrheniusFactor",
                            "[ okin:value ?ArrheniusFactorValue ; okin:unit ?ArrheniusFactorUnit ]",
                        ),
                        (
                            "okin:hasTemperatureExponent/okin:value",
                            "?TemperatureExponentValue",
                        ),
                    ],
                ),
            ]
            arrheniusmodel_vars = ["?ActivationEnergyValue", "?ActivationEnergyUnit", "?ArrheniusFactorValue", "?ArrheniusFactorUnit", "?TemperatureExponentialValue"]
            
            multiarrheniusmodel_patterns = [
                ValuesClause(
                    var="?KineticModelType", values=["okin:MultiArrheniusModel"]
                ),
                TriplePattern.from_triple("?KineticModel","okin:hasArrheniusModel", "?ArrheniusModel"),
                TriplePattern(
                    subj="?ArrheniusModel",
                    tails=[
                        (
                            "okin:hasActivationEnergy",
                            "[ okin:value ?ActivationEnergyValue ; okin:unit ?ActivationEnergyUnit ]",
                        ),
                        (
                            "okin:hasArrheniusFactor",
                            "[ okin:value ?ArrheniusFactorValue ; okin:unit ?ArrheniusFactorUnit ]",
                        ),
                        (
                            "okin:hasTemperatureExponent/okin:value",
                            "?TemperatureExponentValue",
                        ),
                    ],
                ),
            ]
            multiarrheniusmodel_vars = ["?ArrheniusModel", "?ActivationEnergyValue", "?ActivationEnergyUnit", "?ArrheniusFactorValue", "?ArrheniusFactorUnit", "?TemperatureExponentialValue"]

            falloffmodel_patterns = [
                ValuesClause(
                    var="?KineticModelType",
                    values=[
                        "okin:ThreeBodyReactionModel",
                        "okin:LindemannModel",
                        "okin:TroeModel",
                    ],
                ),
                OptionalClause([
                    TriplePattern.from_triple("?KineticMode", "okin:hasCollider", "[ rdfs:label ?ColliderLabel ; okin:hasEfficiency ?ColliderEfficiency ]")
                ]),
                TriplePattern.from_triple("?KineticModel", "okin:hasArrheniusLowModel", "?ArrheniusLowModel"),
                TriplePattern(
                    subj="?ArrheniusLowModel",
                    tails=[
                        (
                            "okin:hasActivationEnergy",
                            "[ okin:value ?ActivationEnergyLowValue ; okin:unit ?ActivationEnergyLowUnit ]",
                        ),
                        (
                            "okin:hasArrheniusFactor",
                            "[ okin:value ?ArrheniusFactorLowValue ; okin:unit ?ArrheniusFactorLowUnit ]",
                        ),
                        (
                            "okin:hasTemperatureExponent/okin:value",
                            "?TemperatureExponentLowValue",
                        ),
                    ],
                ),
                OptionalClause([
                    TriplePattern.from_triple("?KineticModel", "okin:hasArrheniusLowModel", "?ArrheniusHighModel"),
                    TriplePattern(
                        subj="?ArrheniusHighModel",
                        tails=[
                            (
                                "okin:hasActivationEnergy",
                                "[ okin:value ?ActivationEnergyHighValue ; okin:unit ?ActivationEnergyHighUnit ]",
                            ),
                            (
                                "okin:hasArrheniusFactor",
                                "[ okin:value ?ArrheniusFactorHighValue ; okin:unit ?ArrheniusFactorHighUnit ]",
                            ),
                            (
                                "okin:hasTemperatureExponent/okin:value",
                                "?TemperatureExponentHighValue",
                            ),
                        ],
                    ),
                    OptionalClause([
                        TriplePattern(
                        subj="?KineticModel",
                        tails=[
                            ("okin:hasAlpha/okin:value", "?AlphaValue"),
                            ("okin:hasT1/okin:value", "?T1Value"),
                            ("okin:hasT2/okin:value", "?T2Value"),
                            ("okin:hasT3/okin:value", "?T3Value"),
                        ],
                    ),
                    ])
                ]),
            ]
            falloffmodel_vars = [
                "?ColliderLabel", "?ColliderEfficiency", 
                "?ActivationEnergyLowValue", "?ActivationEnergyLowUnit", "?ArrheniusFactorLowValue", "?ArrheniusFactorLowUnit", "?TemperatureExponentLowValue",
                "?ActivationEnergyHighValue", "?ActivationEnergyHighUnit", "?ArrheniusFactorHighValue", "?ArrheniusFactorHighUnit", "?TemperatureExponentHighValue",
                "?AlphaValue", "?T1Value", "?T2Value", "?T3Value"
            ]

            patterns: List[GraphPattern] = [
                pattern,
                TriplePattern.from_triple("?KineticModel", "a", "?KineticModelType"),
                BindClause(exprn='STRAFTER(STR(?KineticModelType), "#")', var="?ModelType"),
                OptionalClause(arrheniusmodel_patterns),
                OptionalClause(multiarrheniusmodel_patterns),
                OptionalClause(falloffmodel_patterns),
            ]
            select_vars = ["?ModelType"] + list(set(arrheniusmodel_vars).union(set(multiarrheniusmodel_vars)).union(set(falloffmodel_vars)))

            return patterns, select_vars
        except AssertionError:
            return None

    def convert(self, sparql_compact: SparqlQuery):
        graph_patterns = list(sparql_compact.graph_patterns)
        graph_patterns.reverse()

        select_vars_verbose = list(sparql_compact.select_clause.vars)
        graph_patterns_verbose = []

        patterns, select_vars = self.add_type_and_label_for_select_entities(sparql_compact)
        graph_patterns_verbose.extend(patterns)
        select_vars_verbose.extend(select_vars)

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

            optional = self._try_convert_rxn_haskineticmodel_triple(pattern)
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
