from typing import List, Literal, Optional, Set

from services.translate.sparql import SparqlQuery
from services.translate.sparql.query_form import SelectClause
from services.translate.sparql.graph_pattern import (
    BindClause,
    FilterClause,
    GraphPattern,
    OptionalClause,
    ServicePattern,
    TriplePattern,
    ValuesClause,
)
from services.translate.sparql.where_clause import WhereClause
from services.translate.sparql.solution_modifier import SolutionModifier, GroupClause


class OKSparqlCompact2VerboseConverter:
    def __init__(self, ontospecies_endpoint: str):
        self.os_endpoint = ontospecies_endpoint

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
                isinstance(pattern, ValuesClause)
                and pattern.var == "?Species"
                # isinstance(pattern, TriplePattern)
                # and pattern.subj == "?Species"
                # and any(pred == "skos:altLabel" for pred, _ in pattern.tails)
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
                and any(
                    pred == "okin:hasProvenance/op:hasDOI" for pred, _ in pattern.tails
                )
            )

        if topic_entity_type == "Species":
            checker = is_species_grounded
        elif topic_entity_type == "Reaction":
            checker = is_rxn_grounded
        elif topic_entity_type == "Mechanism":
            checker = is_mechanism_grounded
        else:
            checker = lambda _: False

        return any(checker(pattern) for pattern in query.where_clause.graph_patterns)

    def add_type_and_label_for_select_entities(self, query: SparqlQuery):
        okin_patterns = []
        os_patterns = []
        select_vars = []

        entity_types = self._get_types_topicentities_selectvars(query)
        for entity_type in entity_types:
            is_entity_grounded = self._is_entity_grounded(query, entity_types)
            if entity_type == "Species":
                if is_entity_grounded:
                    okin_patterns.append(
                        TriplePattern.from_triple(
                            "?Species", "a/rdfs:subClassOf*", "os:Species"
                        )
                    )
                else:
                    okin_patterns.append(
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
                    okin_patterns.append(
                        TriplePattern.from_triple(
                            "?Reaction", "a", "okin:GasPhaseReaction"
                        )
                    )
                else:
                    okin_patterns.append(
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
                    okin_patterns.append(
                        TriplePattern.from_triple(
                            "?Mechanism", "a", "okin:ReactionMechanism"
                        )
                    )
                else:
                    okin_patterns.append(
                        TriplePattern(
                            subj="?Mechanism",
                            tails=[
                                ("a", "okin:ReactionMechanism"),
                                ("okin:hasProvenance/(op:hasDOI|op:hasURL)", "?DOI"),
                            ],
                        )
                    )
                    select_vars.append("?DOI")

        return okin_patterns, os_patterns, select_vars

    def _try_convert_hasChemicalClass(self, pattern: GraphPattern):
        """?Species os:hasChemicalClass/rdfs:label "XXX" ."""
        if not isinstance(pattern, TriplePattern) or len(pattern.tails) != 1:
            return None

        pred, obj = pattern.tails[0]
        if pred != "os:hasChemicalClass/rdfs:label":
            return None

        """
        SERVICE <ontospecies_endpoint> {
            ?Species_ a os:Species ; rdfs:label ?SpeciesLabel .
            ?Species_ (<>|!<>)+ [ rdf:type os:ChemicalClass ; rdfs:label "organic radical" ] .
        }
        FILTER ( ?Species = ?Species_ )
        """
        okin_patterns = [FilterClause("( {subj} = {subj}_ )".format(subj=pattern.subj))]
        os_patterns = [
            TriplePattern(
                subj=pattern.subj + "_",
                tails=[("a", "os:Species"), ("rdfs:label", "?SpeciesLabel")],
            ),
            TriplePattern.from_triple(
                pattern.subj + "_",
                "(<>|!<>)+",
                "[ rdf:type os:ChemicalClass ; rdfs:label {obj} ]".format(obj=obj),
            ),
        ]
        return [pattern.subj], okin_patterns, os_patterns

    def _try_convert_values_species(self, pattern: GraphPattern):
        """VALUES ?Species { "XXX" }"""
        if (
            not isinstance(pattern, ValuesClause)
            or pattern.var != "?Species"
            or len(pattern.values) != 1
        ):
            return None

        """
        ?Species skos:altLabel "XXX" .
        """
        okin_patterns = [
            TriplePattern.from_triple(
                "?Species", "skos:altLabel", '"{label}"'.format(label=pattern.values[0])
            )
        ]
        return ["?Species"], okin_patterns, []

    def _try_convert_reverseBelongsToPhase_species(self, pattern: GraphPattern):
        """?Mechanism okin:hasGasPhase/^okin:belongsToPhase "XXX" ."""
        if not isinstance(pattern, TriplePattern) or len(pattern.tails) != 1:
            return None

        pred, obj = pattern.tails[0]
        if (
            not pred.endswith("^okin:belongsToPhase")
            or not obj.startswith('"')
            or not obj.endswith('"')
        ):
            return None

        """
        ?Mechanism okin:hasGasPhase/^okin:belongsToPhase ?SpeciesO .
        ?SpeciesO skos:altLabel "XXX" .
        """
        label = obj[1:-1]
        okin_patterns = [
            TriplePattern.from_triple(pattern.subj, pred, "?Species" + label),
            TriplePattern.from_triple("?Species" + label, "skos:altLabel", obj),
        ]

        return ["?Species" + label], okin_patterns, []

    def _try_convert_hasReactantOrProduct_species(self, pattern: GraphPattern):
        """?Reaction (ocape:hasReactant|ocape:hasProduct)/skos:altLabel "XXX ."""
        if not isinstance(pattern, TriplePattern) or len(pattern.tails) != 1:
            return None

        pred, obj = pattern.tails[0]
        try:
            pfirst = next(
                p
                for p in [
                    "(ocape:hasReactant|ocape:hasProduct)",
                    "ocape:hasReactant",
                    "ocape:hasProduct",
                ]
                if pred.startswith(p + "/")
            )
        except StopIteration:
            return None

        """
        ?Reaction ocape:hasReactant|ocape:hasProduct ?SpeciesXXX .
        ?SpeciesXXX skos:altLabel "XXX" .
        """
        plast = pred[len(pfirst) + 1 :]
        label = "".join(c for c in obj if c.isalnum())
        okin_patterns = [
            TriplePattern.from_triple(pattern.subj, pfirst, "?Species" + label),
            TriplePattern.from_triple("?Species" + label, plast, obj),
        ]

        return ["?Species" + label], okin_patterns, []

    def _try_convert_species_hasthermomodel_triple(self, pattern: GraphPattern):
        """?Species okin:hasThermoModel ?ThermoModel ."""
        if not (
            isinstance(pattern, TriplePattern)
            and pattern.subj == "?Species"
            and len(pattern.tails) == 1
        ):
            return None

        predicate, obj = pattern.tails[0]
        if not (predicate == "okin:hasThermoModel" and obj == "?ThermoModel"):
            return None

        """
        ?Species okin:hasThermoModel ?ThermoModel .
        ?ThermoModel 
            okin:hasPolynomial [
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
        okin_patterns = [
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
        return select_vars, okin_patterns, []

    def _try_convert_species_hastransportmodel_triple(self, pattern: GraphPattern):
        """?Species okin:hasTransportModel ?TransportModel ."""
        if not (
            isinstance(pattern, TriplePattern)
            and pattern.subj == "?Species"
            and len(pattern.tails) == 1
        ):
            return None

        predicate, obj = pattern.tails[0]
        if not (predicate == "okin:hasTransportModel" and obj == "?TransportModel"):
            return None

        """
        ?Species okin:hasTransportModel ?TransportModel .
        ?TransportModel okin:hasDipoleMoment [ okin:value ?DipoleMomentValue ; okin:unit ?DipoleMomentUnit ] ;
                        okin:hasLJCollisionDiameter [ okin:value ?LJColissionDiameterValue ; okin:unit ?LJColissionDiameterUnit ] ;
                        okin:hasLJPotentialWellDepth [ okin:value ?LJPotentialWellDepthValue ; okin:unit ?LJPotentialWellDepthUnit ] ;
                        okin:hasPolarizability [ okin:value ?PolarizabilityValue ; okin:unit ?PolarizabilityUnit ] ;
                        okin:hasRotationalRelaxationCollisionNumber/okin:value ?RotationalRelaxationCollisionNumberValue ;
                        okin:hasShapeIndex/okin:value ?ShapeIndexValue .
        """
        okin_patterns = [
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
        return select_vars, okin_patterns, []

    def _try_convert_rxn_haskineticmodel_triple(self, pattern: GraphPattern):
        """?Reaction okin:hasKineticModel ?KineticModel ."""
        if not (
            isinstance(pattern, TriplePattern)
            and pattern.subj == "?Reaction"
            and len(pattern.tails) == 1
        ):
            return None

        predicate, obj = pattern.tails[0]
        if not (predicate == "okin:hasKineticModel" and obj == "?KineticModel"):
            return None

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
        OPTIONAL {
            VALUES ?KineticModelType { okin:PDepArrheniusModel }
            ?KineticModel okin:hasArrheniusModel ?ArrheniusModel .
            ?Pressure okin:isPressureConditionOf ?ArrheniusModel ; okin:value ?PressureValue .
            ?ArrheniusModel okin:hasActivationEnergy [ okin:value ?ActivationEnergyValue ; okin:unit ?ActivationEnergyUnit ] ; 
                            okin:hasArrheniusFactor [ okin:value ?ArrheniusFactorValue ; okin:unit ?ArrheniusFactorUnit ] ; 
                            okin:hasTemperatureExponent/okin:value ?TemperatureExponentValue .
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
        arrheniusmodel_vars = [
            "?ActivationEnergyValue",
            "?ActivationEnergyUnit",
            "?ArrheniusFactorValue",
            "?ArrheniusFactorUnit",
            "?TemperatureExponentValue",
        ]

        multiarrheniusmodel_patterns = [
            ValuesClause(var="?KineticModelType", values=["okin:MultiArrheniusModel"]),
            TriplePattern.from_triple(
                "?KineticModel", "okin:hasArrheniusModel", "?ArrheniusModel"
            ),
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
        multiarrheniusmodel_vars = [
            "?ArrheniusModel",
            "?ActivationEnergyValue",
            "?ActivationEnergyUnit",
            "?ArrheniusFactorValue",
            "?ArrheniusFactorUnit",
            "?TemperatureExponentValue",
        ]

        falloffmodel_patterns = [
            ValuesClause(
                var="?KineticModelType",
                values=[
                    "okin:ThreeBodyReactionModel",
                    "okin:LindemannModel",
                    "okin:TroeModel",
                ],
            ),
            OptionalClause(
                [
                    TriplePattern.from_triple(
                        "?KineticMode",
                        "okin:hasCollider",
                        "[ rdfs:label ?ColliderLabel ; okin:hasEfficiency ?ColliderEfficiency ]",
                    )
                ]
            ),
            TriplePattern.from_triple(
                "?KineticModel", "okin:hasArrheniusLowModel", "?ArrheniusLowModel"
            ),
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
            OptionalClause(
                [
                    TriplePattern.from_triple(
                        "?KineticModel",
                        "okin:hasArrheniusLowModel",
                        "?ArrheniusHighModel",
                    ),
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
                    OptionalClause(
                        [
                            TriplePattern(
                                subj="?KineticModel",
                                tails=[
                                    ("okin:hasAlpha/okin:value", "?AlphaValue"),
                                    ("okin:hasT1/okin:value", "?T1Value"),
                                    ("okin:hasT2/okin:value", "?T2Value"),
                                    ("okin:hasT3/okin:value", "?T3Value"),
                                ],
                            ),
                        ]
                    ),
                ]
            ),
        ]
        falloffmodel_vars = [
            "?ColliderLabel",
            "?ColliderEfficiency",
            "?ActivationEnergyLowValue",
            "?ActivationEnergyLowUnit",
            "?ArrheniusFactorLowValue",
            "?ArrheniusFactorLowUnit",
            "?TemperatureExponentLowValue",
            "?ActivationEnergyHighValue",
            "?ActivationEnergyHighUnit",
            "?ArrheniusFactorHighValue",
            "?ArrheniusFactorHighUnit",
            "?TemperatureExponentHighValue",
            "?AlphaValue",
            "?T1Value",
            "?T2Value",
            "?T3Value",
        ]

        pdeparrheniusmodel_patterns = [
            ValuesClause(var="?KineticModelType", values=["okin:PDepArrheniusModel"]),
            TriplePattern.from_triple(
                "?KineticModel", "okin:hasArrheniusModel", "?ArrheniusModel"
            ),
            TriplePattern(
                subj="?Pressure",
                tails=[
                    ("okin:isPressureConditionOf", "?ArrheniusModel"),
                    ("okin:value", "?PressureValue"),
                ],
            ),
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
        pdeparrheniusmodel_vars = [
            "?PressureValue",
            "?ActivationEnergyValue",
            "?ActivationEnergyUnit",
            "?ArrheniusFactorValue",
            "?ArrheniusFactorUnit",
            "?TemperatureExponentValue",
        ]

        okin_patterns: List[GraphPattern] = [
            pattern,
            TriplePattern.from_triple("?KineticModel", "a", "?KineticModelType"),
            BindClause(exprn='STRAFTER(STR(?KineticModelType), "#")', var="?ModelType"),
            OptionalClause(arrheniusmodel_patterns),
            OptionalClause(multiarrheniusmodel_patterns),
            OptionalClause(falloffmodel_patterns),
            OptionalClause(pdeparrheniusmodel_patterns),
        ]
        select_vars = ["?ModelType"] + list(
            set(arrheniusmodel_vars)
            .union(set(multiarrheniusmodel_vars))
            .union(set(falloffmodel_vars))
            .union(set(pdeparrheniusmodel_vars))
        )

        return select_vars, okin_patterns, []

    def _try_convert_hasProvenanceDOI(self, pattern: GraphPattern):
        """?Mechanism okin:hasProvenance/op:hasDOI "XXX" ."""
        if not isinstance(pattern, TriplePattern) or len(pattern.tails) != 1:
            return None

        pred, obj = pattern.tails[0]
        if pred != "okin:hasProvenance/op:hasDOI":
            return None

        okin_patterns = [
            TriplePattern.from_triple(
                pattern.subj, "okin:hasProvenance/(op:hasDOI|op:hasURL)", obj
            )
        ]

        return [], okin_patterns, []

    def _sample_rxn_eqn(self, query: SparqlQuery):
        if "?ReactionEquation" not in query.select_clause.vars:
            return query

        vars = [
            (
                x
                if x != "?ReactionEquation"
                else "(SAMPLE(?ReactionEquation) AS ?SampledReactionEquation)"
            )
            for x in query.select_clause.vars
        ]
        return SparqlQuery(
            select_clause=SelectClause(
                vars=vars, solution_modifier=query.select_clause.solution_modifier
            ),
            where_clause=query.where_clause,
            solution_modifier=SolutionModifier(
                group_clause=GroupClause(
                    (
                        query.solution_modifier.group_clause.vars
                        if query.solution_modifier
                        and query.solution_modifier.group_clause
                        else []
                    )
                    + [x for x in query.select_clause.vars if x != "?ReactionEquation"]
                ),
                order_clause=(
                    query.solution_modifier.order_clause
                    if query.solution_modifier and query.solution_modifier.order_clause
                    else None
                ),
                limit_clause=(
                    query.solution_modifier.limit_clause
                    if query.solution_modifier and query.solution_modifier.limit_clause
                    else None
                ),
            ),
        )

    def convert(self, sparql_compact: SparqlQuery):
        graph_patterns = list(sparql_compact.where_clause.graph_patterns)
        graph_patterns.reverse()

        select_vars_verbose = list(sparql_compact.select_clause.vars)
        graph_patterns_verbose, service_os_patterns, select_vars = (
            self.add_type_and_label_for_select_entities(sparql_compact)
        )
        select_vars_verbose.extend(select_vars)

        while len(graph_patterns) > 0:
            pattern = graph_patterns.pop()

            found = False
            for func in [
                self._try_convert_hasChemicalClass,
                self._try_convert_values_species,
                self._try_convert_reverseBelongsToPhase_species,
                self._try_convert_hasReactantOrProduct_species,
                self._try_convert_species_hasthermomodel_triple,
                self._try_convert_species_hastransportmodel_triple,
                self._try_convert_rxn_haskineticmodel_triple,
                self._try_convert_hasProvenanceDOI,
            ]:
                optional = func(pattern)
                if optional is not None:
                    found = True
                    select_vars, okin_patterns, os_patterns = optional
                    select_vars_verbose.extend(select_vars)
                    graph_patterns_verbose.extend(okin_patterns)
                    service_os_patterns.extend(os_patterns)
                    break

            if not found:
                graph_patterns_verbose.append(pattern)

        if service_os_patterns:
            graph_patterns_verbose.append(
                ServicePattern(
                    endpoint=self.os_endpoint, graph_patterns=tuple(service_os_patterns)
                )
            )

        sparql_verbose = SparqlQuery(
            select_clause=SelectClause(
                solution_modifier="DISTINCT", vars=select_vars_verbose
            ),
            where_clause=WhereClause(graph_patterns_verbose),
            solution_modifier=sparql_compact.solution_modifier,
        )

        return self._sample_rxn_eqn(sparql_verbose)
