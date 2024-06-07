from collections import defaultdict
from typing import Sequence

from services.kg import KgClient


# TODO: ORM
def get_thermo_model_data(kg_client: KgClient, iris: Sequence[str]):
    query = """PREFIX okin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>

SELECT *
WHERE {{
    VALUES ?ThermoModel {{ {values} }}
    ?ThermoModel
        okin:hasTmin [ okin:value ?TminValue ; okin:unit ?TminUnit ] ;
        okin:hasTmax [ okin:value ?TmaxValue ; okin:unit ?TmaxUnit ] .
}}""".format(
        values=" ".join("<{iri}>".format(iri=iri) for iri in iris)
    )

    _, bindings  = kg_client.querySelectThenFlatten(query)

    iri2trange = {
        binding["ThermoModel"]: {
            key: {"value": binding[key + "Value"], "unit": binding[key + "Unit"]}
            for key in ["Tmin", "Tmax"]
        }
        for binding in bindings
    }

    query = """PREFIX okin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>

SELECT *
WHERE {{
    VALUES ?ThermoModel {{ {values} }}
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
        values=" ".join("<{iri}>".format(iri=iri) for iri in iris)
    )

    _, bindings  = kg_client.querySelectThenFlatten(query)

    iri2polys = defaultdict(list)
    for binding in bindings:
        iri2polys[binding["ThermoModel"]].append(
            {
                **{
                    key: {
                        "value": binding[key + "Value"],
                        "unit": binding[key + "Unit"],
                    }
                    for key in ["Tmin", "Tmax"]
                },
                **{
                    key: binding[key]
                    for key in ["A1", "A2", "A3", "A4", "A5", "A6", "A7", "B1", "B2"]
                },
            }
        )

    return [
        {**iri2trange.get(iri, {}), "Polynomials": iri2polys.get(iri, [])}
        for iri in iris
    ]


def get_transport_model_data(kg_client: KgClient, iris: Sequence[str]):
    query = """PREFIX okin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>

SELECT *
WHERE {{
    VALUES ?TransportModel {{ {values} }}
    ?TransportModel 
        okin:hasDipoleMoment [ okin:value ?DipoleMomentValue ; okin:unit ?DipoleMomentUnit ] ;
        okin:hasLJCollisionDiameter [ okin:value ?LJColissionDiameterValue ; okin:unit ?LJColissionDiameterUnit ] ;
        okin:hasLJPotentialWellDepth [ okin:value ?LJPotentialWellDepthValue ; okin:unit ?LJPotentialWellDepthUnit ] ;
        okin:hasPolarizability [ okin:value ?PolarizabilityValue ; okin:unit ?PolarizabilityUnit ] ;
        okin:hasRotationalRelaxationCollisionNumber/okin:value ?RotationalRelaxationCollisionNumberValue ;
        okin:hasShapeIndex/okin:value ?ShapeIndexValue .
}}""".format(
        values=" ".join("<{iri}>".format(iri=iri) for iri in iris)
    )

    _, bindings  = kg_client.querySelectThenFlatten(query)

    iri2data = {
        binding["TransportModel"]: {
            **{
                key: {"value": binding[key + "Value"], "unit": binding[key + "Unit"]}
                for key in [
                    "DipoleMoment",
                    "LJCollisionDiameter",
                    "LJPotentialWellDepth",
                    "Polarizability",
                    "RotationalRelaxationCollisionNumber",
                ]
            },
            "ShapeIndex": binding["ShapeIndexValue"],
        }
        for binding in bindings
    }

    return [iri2data.get(iri, {}) for iri in iris]
