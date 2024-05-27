from functools import cache
from typing import Annotated, Optional

from fastapi import Depends

from constants.prefixes import URI_OKIN
from services.kg import KgClient, get_ontokin_bgClient
from services.processs_response.utils import iri_slot_template_to_func
from utils.rdf import flatten_sparql_response
from .expand_response import SparqlResponseExpander


# TODO: Refactor using an RDF-ORM library
def get_thermo_model_data(iri: str, kg_client: KgClient):
    query = """PREFIX okin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>

SELECT *
WHERE {{
    VALUES ?ThermoModel {{ <{IRI}> }}
    ?ThermoModel
        okin:hasTmin [ okin:value ?TminValue ; okin:unit ?TminUnit ] ;
        okin:hasTmax [ okin:value ?TmaxValue ; okin:unit ?TmaxUnit ] ;
}}""".format(IRI=iri)

    res = kg_client.query(query)
    _, bindings = flatten_sparql_response(res)

    if bindings:
        binding = bindings[0]
        tmin = {"value": binding["TminValue"], "unit": binding["TminUnit"]}
        tmax = {"value": binding["TmaxValue"], "unit": binding["TmaxUnit"]}
    else:
        tmin = None
        tmax = None

    query = """PREFIX okin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>

SELECT *
WHERE {{
    VALUES ?ThermoModel {{ <{IRI}> }}
    ?ThermoModel okin:hasPolynomial [
            okin:hasTmin [ okin:value ?TminValue ; okin:unit ?TminUnit ] ;
            okin:hasTmax [ okin:value ?TmaxValue ; okin:unit ?TmaxUnit ] ;
            okin:hasA1 ?A1 ;
            okin:hasA2 ?A2 ;
            okin:hasA3 ?A3 ;
            okin:hasA4 ?A4 ;
            okin:hasA5 ?A5 ;
            okin:hasA6 ?A6 ;
            okin:hasA7 ?A7 ;
            okin:hasB1 ?B1 ;
            okin:hasB2 ?B2
        ] .
}}""".format(
        IRI=iri
    )

    res = kg_client.query(query)
    _, bindings = flatten_sparql_response(res)

    polys = [
        {
            "Tmin": {"value": binding["TminValue"], "unit": binding["TminUnit"]},
            "Tmax": {"value": binding["TmaxValue"], "unit": binding["TmaxUnit"]},
            "A1": binding["A1"],
            "A2": binding["A2"],
            "A3": binding["A3"],
            "A4": binding["A4"],
            "A5": binding["A5"],
            "A6": binding["A6"],
            "A7": binding["A7"],
            "B1": binding["B1"],
            "B2": binding["B2"],
        }
        for binding in bindings
    ]

    data = {"Tmin": tmin, "Tmax": tmax, "Polynomials": polys}
    data = {k: v for k, v in data.items() if v}

    return data or None


def get_transport_model_data(iri: str, kg_client: KgClient):
    query = """PREFIX okin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>

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
}}""".format(
        IRI=iri
    )

    res = kg_client.query(query)
    _, bindings = flatten_sparql_response(res)

    if not bindings:
        return None

    binding = bindings[0]
    return {
        "DipoleMoment": {
            "value": binding["DipoleMomentValue"],
            "unit": binding["DipoleMomentUnit"],
        },
        "LJCollisionDiameter": {
            "value": binding["LJColissionDiameterValue"],
            "unit": binding["LJColissionDiameterUnit"],
        },
        "LJPotentialWellDepth": {
            "value": binding["LJPotentialWellDepthValue"],
            "unit": binding["LJPotentialWellDepthUnit"],
        },
        "Polarizability": {
            "value": binding["PolarizabilityValue"],
            "unit": binding["PolarizabilityUnit"],
        },
        "RotationalRelaxationCollisionNumber": binding[
            "RotationalRelaxationCollisionNumberValue"
        ],
        "ShapeIndex": binding["ShapeIndexValue"],
    }


def _get_arrhenius_models(
    iri: str, bg_client: KgClient, predicate: Optional[str] = None
):
    query = """PREFIX okin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>

SELECT *
WHERE {{
    VALUES ?IRI {{ <{IRI}> }}
    {path}
    ?ArrheniusModel
        okin:hasActivationEnergy [ okin:value ?ActivationEnergyValue ; okin:unit ?ActivationEnergyUnit ] ; 
        okin:hasArrheniusFactor [ okin:value ?ArrheniusFactorValue ; okin:unit ?ArrheniusFactorUnit ] ; 
        okin:hasTemperatureExponent/okin:value ?TemperatureExponentValue .
    OPTIONAL {{
        ?Pressure okin:isPressureConditionOf ?ArrheniusModel ; okin:value ?PressureValue .
    }}
}}""".format(
        path=(
            "?IRI {predicate} ?ArrheniusModel .".format(predicate=predicate)
            if predicate
            else "BIND ( ?IRI AS ?ArrheniusModel )"
        ),
        IRI=iri,
    )

    res = bg_client.query(query)
    _, bindings = flatten_sparql_response(res)

    data = [
        {
            "Pressure": binding["PressureValue"],
            "ActivationEnergy": {
                "value": binding["ActivationEnergyValue"],
                "unit": binding["ActivationEnergyUnit"],
            },
            "ArrheniusFactor": {
                "value": binding["ArrheniusFactorValue"],
                "unit": binding["ArrheniusFactorUnit"],
            },
            "TemperatureExponent": binding["TemperatureExponentValue"],
        }
        for binding in bindings
    ]
    return [{k: v for k, v in binding.items() if v} for binding in data]


def _get_arrhenius_model_first(
    iri: str, bg_client: KgClient, predicate: Optional[str] = None
):
    models = _get_arrhenius_models(iri=iri, bg_client=bg_client, predicate=predicate)
    if models:
        return models[0]
    else:
        return None


def get_kinetic_model_data_arrhenius(iri: str, kg_client: KgClient):
    data = _get_arrhenius_model_first(iri=iri, bg_client=kg_client) or {}

    return {
        "Type": "Arrhenius model",
        "Data": data,
    }


def get_kinetic_model_data_multi_arrhenius(iri: str, kg_client: KgClient):
    data = {
        "ArrheniusModels": _get_arrhenius_models(
            iri=iri, bg_client=kg_client, predicate="okin:hasArrheniusModel"
        )
    }

    return {
        "Type": "Multi-Arrhenius model",
        "Data": {k: v for k, v in data.items() if v},
    }


def _get_colliders(iri: str, bg_client: KgClient, predicate: Optional[str] = None):
    query = """PREFIX okin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>

SELECT *
WHERE {{
    VALUES ?IRI {{ <{IRI}> }}
    {path}
    ?Collider rdfs:label ?Label ; okin:hasEfficiency ?Efficiency .
}}""".format(
        path=(
            "?IRI {predicate} ?Collider .".format(predicate=predicate)
            if predicate
            else "BIND ( ?IRI AS ?Collider )"
        ),
        IRI=iri,
    )

    res = bg_client.query(query)
    _, bindings = flatten_sparql_response(res)

    return [
        {"Label": binding["Label"], "Efficiency": binding["Efficiency"]}
        for binding in bindings
    ]


def get_kinetic_model_data_three_body_rxn(iri: str, kg_client: KgClient):
    data = {
        "Colliders": _get_colliders(
            iri=iri, bg_client=kg_client, predicate="okin:hasCollider"
        ),
        "ArrheniusLowModel": _get_arrhenius_model_first(
            iri=iri, bg_client=kg_client, predicate="okin:hasArrheniusLowModel"
        ),
    }

    return {"Type": "Three-body reaction", "Data": {k: v for k, v in data.items() if v}}


def get_kinetic_model_data_lindemann(iri: str, kg_client: KgClient):
    data = {
        "Colliders": _get_colliders(
            iri=iri, bg_client=kg_client, predicate="okin:hasCollider"
        ),
        "ArrheniusLowModel": _get_arrhenius_model_first(
            iri=iri, bg_client=kg_client, predicate="okin:hasArrheniusLowModel"
        ),
        "ArrheniusHighModel": _get_arrhenius_model_first(
            iri=iri, bg_client=kg_client, predicate="okin:hasArrheniusHighModel"
        ),
    }

    return {"Type": "Lindemann model", "Data": {k: v for k, v in data.items() if v}}


def get_kinetic_model_data_troe(iri: str, kg_client: KgClient):
    query = """PREFIX okin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>

SELECT *
WHERE {{
    VALUES ?TroeModel {{ <{IRI}> }}
    ?TroeModel
        okin:hasAlpha/okin:value ?Alpha ;
        okin:hasT1/okin:value ?T1 ;
        okin:hasT2/okin:value ?T2 ;
        okin:hasT3/okin:value ?T3 .
}}""".format(
        IRI=iri,
    )

    res = kg_client.query(query)
    _, bindings = flatten_sparql_response(res)

    if not bindings:
        troe_data = {}
    else:
        troe_data = bindings[0]

    data = {
        "Colliders": _get_colliders(
            iri=iri, bg_client=kg_client, predicate="okin:hasCollider"
        ),
        **troe_data,
        "ArrheniusLowModel": _get_arrhenius_model_first(
            iri=iri, bg_client=kg_client, predicate="okin:hasArrheniusLowModel"
        ),
        "ArrheniusHighModel": _get_arrhenius_model_first(
            iri=iri, bg_client=kg_client, predicate="okin:hasArrheniusHighModel"
        ),
    }

    return {"Type": "Troe model", "Data": {k: v for k, v in data.items() if v}}


def get_kinetic_model_data_pdep_arrhenius(iri: str, kg_client: KgClient):
    data = {
        "ArrheniusModels": _get_arrhenius_models(
            iri=iri, bg_client=kg_client, predicate="okin:hasArrheniusModel"
        )
    }
    return {
        "Type": "Pressure-dependent Arrhenius model",
        "Data": {k: v for k, v in data.items() if v},
    }


ONTOKIN_TYPE2GETTER = {
    URI_OKIN + "ThermoModel": get_thermo_model_data,
    URI_OKIN + "TransportModel": get_transport_model_data,
    URI_OKIN + "ArrheniusModel": get_kinetic_model_data_arrhenius,
    URI_OKIN + "MultiArrheniusModel": get_kinetic_model_data_multi_arrhenius,
    URI_OKIN + "ThreeBodyReactionModel": get_kinetic_model_data_three_body_rxn,
    URI_OKIN + "LindemannModel": get_kinetic_model_data_lindemann,
    URI_OKIN + "TroeModel": get_kinetic_model_data_troe,
    URI_OKIN + "PDepArrheniusModel": get_kinetic_model_data_pdep_arrhenius,
}


@cache
def get_ontokin_responseExpander(
    bg_client: Annotated[KgClient, Depends(get_ontokin_bgClient)]
):
    return SparqlResponseExpander(kg_client=bg_client, type2getter=ONTOKIN_TYPE2GETTER)
