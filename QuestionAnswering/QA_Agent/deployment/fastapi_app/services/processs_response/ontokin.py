from functools import cache
from typing import Annotated

from fastapi import Depends

from services.kg import KgClient, get_ontokin_bgClient
from services.processs_response.utils import iri_slot_template_to_func
from .exapnd_response import SparqlResponseExpander


THERMO_MODEL_IRI = (
    "http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#ThermoModel"
)
TRANSPORT_MODEL_IRI = (
    "http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#TransportModel"
)
KINETIC_MODEL_IRI = (
    "http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#KineticModel"
)

THERMO_MODEL_TRIPLES_TEMPLATE = """PREFIX okin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>

SELECT *
WHERE {{
    VALUES ?ThermoModel {{ <{IRI}> }}
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
}}"""

TRANSPORT_MODEL_TRIPLES = """PREFIX okin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>

SELECT *
WHERE {{
    VALUES ?TransportModel {{ <{IRI}> }}
    ?TransportModel 
        okin:hasDipoleMoment [ okin:value ?DipoleMomentValue ; okin:unit ?DipoleMomentUnit ] ;
        okin:hasLJCollisionDiameter [ okin:value ?LJColissionDiameterValue ; okin:unit ?LJColissionDiameterUnit ] ;
        okin:hasLJPotentialWellDepth [ okin:value ?LJPotentialWellDepthValue ; okin:unit ?LJPotentialWellDepthUnit ] ;
        okin:hasPolarizability [ okin:value ?PolarizabilityValue ; okin:unit ?PolarizabilityUnit ] ;
        okin:hasRotationalRelaxationCollisionNumber/okin:value ?RotationalRelaxationCollisionNumberValue ;
        okin:hasShapeIndex/okin:value ?ShapeIndexValue .
}}"""

KINETIC_MODEL_TRIPLES = """PREFIX okin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>

SELECT *
WHERE {{
    VALUES ?KineticModel {{ <{IRI}> }}
    ?KineticModel a ?KineticModelType .
    BIND (STRAFTER(STR(?KineticModelType), "#") AS ?ModelType)
    OPTIONAL {{
        VALUES ?KineticModelType {{ okin:ArrheniusModel }}
        ?KineticModel 
            a okin:ArrheniusModel ; 
            okin:hasActivationEnergy [ okin:value ?ActivationEnergyValue ; okin:unit ?ActivationEnergyUnit ] ; 
            okin:hasArrheniusFactor [ okin:value ?ArrheniusFactorValue ; okin:unit ?ArrheniusFactorUnit ] ; 
            okin:hasTemperatureExponent/okin:value ?TemperatureExponentValue .
    }}
    OPTIONAL {{
        VALUES ?KineticModelType {{ okin:MultiArrheniusModel }}
        ?KineticModel okin:hasArrheniusModel ?ArrheniusModel .
        ?ArrheniusModel 
            okin:hasActivationEnergy [ okin:value ?ActivationEnergyValue ; okin:unit ?ActivationEnergyUnit ] ; 
            okin:hasArrheniusFactor [ okin:value ?ArrheniusFactorValue ; okin:unit ?ArrheniusFactorUnit ] ; 
            okin:hasTemperatureExponent/okin:value ?TemperatureExponentValue .
    }}
    OPTIONAL {{
        VALUES ?KineticModelType {{ okin:ThreeBodyReactionModel okin:LindemannModel okin:TroeModel }}
        OPTIONAL {{
            ?KineticMode okin:hasCollider [ rdfs:label ?ColliderLabel ; okin:hasEfficiency ?ColliderEfficiency ] .
        }}
        ?KineticModel okin:hasArrheniusLowModel ?ArrheniusLowModel .
        ?ArrheniusLowModel
            okin:hasActivationEnergy [ okin:value ?ActivationEnergyLowValue ; okin:unit ?ActivationEnergyLowUnit ] ; 
            okin:hasArrheniusFactor [ okin:value ?ArrheniusFactorLowValue ; okin:unit ?ArrheniusFactorLowUnit ] ; 
            okin:hasTemperatureExponent/okin:value ?TemperatureExponentLowValue .
        OPTIONAL {{
            ?KineticModel okin:hasArrheniusHighModel ?ArrheniusHighModel .
            ?ArrheniusHighModel 
                okin:hasActivationEnergy [ okin:value ?ActivationEnergyHighValue ; okin:unit ?ActivationEnergyHighUnit ] ; 
                okin:hasArrheniusFactor [ okin:value ?ArrheniusFactorHighValue ; okin:unit ?ArrheniusFactorHighUnit ] ; 
                okin:hasTemperatureExponent/okin:value ?TemperatureExponentHighValue .
            OPTIONAL {{
                ?KineticModel 
                    okin:hasAlpha/okin:value ?AlphaValue ; 
                    okin:hasT1/okin:value ?T1Value ; 
                    okin:hasT2/okin:value ?T2Value ; 
                    okin:hasT3/okin:value ?T3Value .
            }}
        }}
    }}
    OPTIONAL {{
        VALUES ?KineticModelType {{ okin:PDepArrheniusModel }}
        ?KineticModel okin:hasArrheniusModel ?ArrheniusModel .
        ?Pressure okin:isPressureConditionOf ?ArrheniusModel ; okin:value ?PressureValue .
        ?ArrheniusModel 
            okin:hasActivationEnergy [ okin:value ?ActivationEnergyValue ; okin:unit ?ActivationEnergyUnit ] ; 
            okin:hasArrheniusFactor [ okin:value ?ArrheniusFactorValue ; okin:unit ?ArrheniusFactorUnit ] ; 
            okin:hasTemperatureExponent/okin:value ?TemperatureExponentValue .
    }}
}}"""

ONTOKIN_TYPE2SPARQL = {
    THERMO_MODEL_IRI: iri_slot_template_to_func(THERMO_MODEL_TRIPLES_TEMPLATE),
    TRANSPORT_MODEL_IRI: iri_slot_template_to_func(TRANSPORT_MODEL_TRIPLES),
    KINETIC_MODEL_IRI: iri_slot_template_to_func(KINETIC_MODEL_TRIPLES),
}


@cache
def get_ontokin_responseExpander(
    bg_client: Annotated[KgClient, Depends(get_ontokin_bgClient)]
):
    return SparqlResponseExpander(
        bg_client=bg_client, type2sparql=ONTOKIN_TYPE2SPARQL
    )
