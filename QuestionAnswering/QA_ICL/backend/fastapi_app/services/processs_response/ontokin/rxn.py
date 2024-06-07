from collections import defaultdict
from typing import Optional, Sequence


from constants.prefixes import URI_OKIN
from services.kg import KgClient
from utils.rdf import flatten_sparql_select_response


# TODO: ORM
def get_rxn_data(kg_client: KgClient, iris: Sequence[str]):
    query = """PREFIX okin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>

SELECT ?Reaction (SAMPLE(?Equation) AS ?Equation)
WHERE {{
    VALUES ?Reaction {{ {values} }}
    ?Reaction okin:hasEquation ?Equation .
}}
GROUP BY ?Reaction""".format(
        values=" ".join("<{iri}>".format(iri=iri) for iri in iris)
    )

    res = kg_client.querySelect(query)
    _, bindings = flatten_sparql_select_response(res)

    iri2data = {
        binding["Reaction"]: {"Equation": binding["Equation"]} for binding in bindings
    }
    return [{"IRI": iri, **iri2data.get(iri, {})} for iri in iris]


def _get_arrhenius_models(
    kg_client: KgClient, iris: Sequence[str], predicate: Optional[str] = None
):
    query = """PREFIX okin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>

SELECT *
WHERE {{
    VALUES ?IRI {{ {values} }}
    {path}
    ?ArrheniusModel
        okin:hasActivationEnergy [ okin:value ?ActivationEnergyValue ; okin:unit ?ActivationEnergyUnit ] ; 
        okin:hasArrheniusFactor [ okin:value ?ArrheniusFactorValue ; okin:unit ?ArrheniusFactorUnit ] ; 
        okin:hasTemperatureExponent/okin:value ?TemperatureExponentValue .
    OPTIONAL {{
        ?Pressure okin:isPressureConditionOf ?ArrheniusModel ; okin:value ?PressureValue .
    }}
}}""".format(
        values=" ".join("<{iri}>".format(iri=iri) for iri in iris),
        path=(
            "?IRI {predicate} ?ArrheniusModel .".format(predicate=predicate)
            if predicate
            else "BIND ( ?IRI AS ?ArrheniusModel )"
        ),
    )

    res = kg_client.querySelect(query)
    _, bindings = flatten_sparql_select_response(res)

    iri2data: defaultdict[str, list[dict]] = defaultdict(list)
    for binding in bindings:
        datum = {
            "Pressure": binding.get("PressureValue"),
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
        datum = {k: v for k, v in datum.items() if v}
        iri2data[binding["IRI"]].append(datum)

    return [iri2data[iri] for iri in iris]


def _get_arrhenius_model_first(
    kg_client: KgClient, iris: Sequence[str], predicate: Optional[str] = None
):
    data = _get_arrhenius_models(kg_client=kg_client, iris=iris, predicate=predicate)
    return [datum[0] if datum else {} for datum in data]


def get_kinetic_model_data_arrhenius(kg_client: KgClient, iris: Sequence[str]):
    data = _get_arrhenius_model_first(kg_client=kg_client, iris=iris)
    return [{"Type": "Arrhenius model", **datum} for datum in data]


def get_kinetic_model_data_multi_arrhenius(kg_client: KgClient, iris: Sequence[str]):
    data = _get_arrhenius_models(
        kg_client=kg_client, iris=iris, predicate="okin:hasArrheniusModel"
    )
    return [
        {"Type": "Multi-Arrhenius model", "ArrheniusModels": datum} for datum in data
    ]


def _get_colliders(
    kg_client: KgClient, iris: Sequence[str], predicate: Optional[str] = None
):
    query = """PREFIX okin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>

SELECT *
WHERE {{
    VALUES ?IRI {{ {values} }}
    {path}
    ?Collider rdfs:label ?Label ; okin:hasEfficiency ?Efficiency .
}}""".format(
        values=" ".format("<{iri}>".format(iri=iri) for iri in iris),
        path=(
            "?IRI {predicate} ?Collider .".format(predicate=predicate)
            if predicate
            else "BIND ( ?IRI AS ?Collider )"
        ),
    )

    res = kg_client.querySelect(query)
    _, bindings = flatten_sparql_select_response(res)

    iri2data: defaultdict[str, list[dict]] = defaultdict(list)
    for binding in bindings:
        iri2data[binding["IRI"]].append(
            {"Label": binding["Label"], "Efficiency": binding["Efficiency"]}
            for binding in bindings
        )

    return [iri2data[iri] for iri in iris]


def get_kinetic_model_data_three_body_rxn(kg_client: KgClient, iris: Sequence[str]):
    collider_data = _get_colliders(
        kg_client=kg_client, iris=iris, predicate="okin:hasCollider"
    )
    arrhenius_data = _get_arrhenius_model_first(
        kg_client=kg_client, iris=iris, predicate="okin:hasArrheniusLowModel"
    )
    return [
        {
            "Type": "Three-body reaction model",
            "Colliders": collider_datum,
            "ArrheniusLowModel": arrhenius_datum,
        }
        for collider_datum, arrhenius_datum in zip(collider_data, arrhenius_data)
    ]


def get_kinetic_model_data_lindemann(kg_client: KgClient, iris: Sequence[str]):
    collider_data = _get_colliders(
        kg_client=kg_client, iris=iris, predicate="okin:hasCollider"
    )
    arrhenius_low_data = _get_arrhenius_model_first(
        kg_client=kg_client, iris=iris, predicate="okin:hasArrheniusLowModel"
    )
    arrhenius_high_data = _get_arrhenius_model_first(
        kg_client=kg_client, iris=iris, predicate="okin:hasArrheniusHighModel"
    )

    return [
        {
            "Type": "Lindemann model",
            "Colliders": collider_datum,
            "ArrheniusLowModel": arrhenius_low_datum,
            "ArrheniusHighModel": arrhenius_high_datum,
        }
        for collider_datum, arrhenius_low_datum, arrhenius_high_datum in zip(
            collider_data, arrhenius_low_data, arrhenius_high_data
        )
    ]


def get_kinetic_model_data_troe(kg_client: KgClient, iris: Sequence[str]):
    collider_data = _get_colliders(
        kg_client=kg_client, iris=iris, predicate="okin:hasCollider"
    )

    query = """PREFIX okin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>

SELECT *
WHERE {{
    VALUES ?TroeModel {{ {values} }}
    ?TroeModel
        okin:hasAlpha/okin:value ?Alpha ;
        okin:hasT1/okin:value ?T1 ;
        okin:hasT2/okin:value ?T2 ;
        okin:hasT3/okin:value ?T3 .
}}""".format(
        values=" ".join("<{iri}>".format(iri=iri) for iri in iris)
    )

    res = kg_client.querySelect(query)
    _, bindings = flatten_sparql_select_response(res)

    iri2troedata = {
        binding["TroeModel"]: {k: v for k, v in binding.items() if k != "TroeModel"}
        for binding in bindings
    }
    troe_data = [iri2troedata.get(iri, {}) for iri in iris]

    arrhenius_low_data = _get_arrhenius_model_first(
        kg_client=kg_client, iris=iris, predicate="okin:hasArrheniusLowModel"
    )
    arrhenius_high_data = _get_arrhenius_model_first(
        kg_client=kg_client, iris=iris, predicate="okin:hasArrheniusHighModel"
    )

    return [
        {
            "Type": "Troe model",
            "Colliders": collider_datum,
            **troe_datum,
            "ArrheniusLowModel": arrhenius_low_datum,
            "ArrheniusHighModel": arrhenius_high_datum,
        }
        for collider_datum, troe_datum, arrhenius_low_datum, arrhenius_high_datum in zip(
            collider_data, troe_data, arrhenius_low_data, arrhenius_high_data
        )
    ]


def get_kinetic_model_data_pdep_arrhenius(kg_client: KgClient, iris: Sequence[str]):
    data = _get_arrhenius_models(
        kg_client=kg_client, iris=iris, predicate="okin:hasArrheniusModel"
    )

    return [
        {"Type": "Pressure-dependent Arrhenius model", "ArrheniusModels": datum}
        for datum in data
    ]


KINETIC_MODEL_TYPE_TO_GETTER = {
    URI_OKIN + k: v
    for k, v in {
        "ArrheniusModel": get_kinetic_model_data_arrhenius,
        "MultiArrheniusModel": get_kinetic_model_data_multi_arrhenius,
        "ThreeBodyReactionModel": get_kinetic_model_data_three_body_rxn,
        "LindemannModel": get_kinetic_model_data_lindemann,
        "TroeModel": get_kinetic_model_data_troe,
        "PDepArrheniusModel": get_kinetic_model_data_pdep_arrhenius,
    }.items()
}


def get_kinetic_model_data(kg_client: KgClient, iris: Sequence[str]):
    query = """SELECT * 
WHERE {{
    VALUES ?KineticModel {{ {values} }}
    ?KineticModel a ?Type .
}}""".format(
        values=" ".join("<{iri}>".format(iri=iri) for iri in iris)
    )

    res = kg_client.querySelect(query)
    _, bindings = flatten_sparql_select_response(res)

    type2iris: defaultdict[str, list] = defaultdict(list)
    for binding in bindings:
        type2iris[binding["Type"]].append(binding["KineticModel"])

    iri2data: dict[str, dict] = dict()
    for type, same_type_iris in type2iris.items():
        data = KINETIC_MODEL_TYPE_TO_GETTER[type](
            kg_client=kg_client, iris=same_type_iris
        )
        iri2data.update({iri: datum for iri, datum in zip(same_type_iris, data)})

    return [iri2data.get(iri, {}) for iri in iris]
