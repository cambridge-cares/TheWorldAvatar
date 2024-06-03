from collections import defaultdict
from functools import cache
from typing import Annotated, Sequence

from fastapi import Depends
from constants.prefixes import URI_OCC
from services.kg import KgClient, get_ontocompchem_bgClient
from services.processs_response.augment_node import NodeDataRetriever
from utils.rdf import flatten_sparql_select_response


# TODO: ORM
def get_optimized_geometry_data(kg_client: KgClient, iris: Sequence[str]):
    query = """PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX occ: <http://www.theworldavatar.com/ontology/ontocompchem/OntoCompChem.owl#>

SELECT *
WHERE {{
    VALUES ?OptimizedGeometry {{ {values} }}
    ?OptimizedGeometry ^os:fromGeometry ?X, ?Y, ?Z .
    ?Atom 
        os:hasXCoordinate ?X ;
        os:hasYCoordinate ?Y ;
        os:hasZCoordinate ?Z .
    ?X os:value ?XValue ; os:unit ?XUnit .
    ?Y os:value ?YValue ; os:unit ?YUnit .
    ?Z os:value ?ZValue ; os:unit ?ZUnit .
}}""".format(
        values=" ".join("<{iri}>".format(iri=iri) for iri in iris)
    )

    res = kg_client.querySelect(query)
    _, bindings = flatten_sparql_select_response(res)

    iri2data: defaultdict[str, list[dict]] = defaultdict(list)
    for binding in bindings:
        iri2data[binding["OptimizedGeometry"]].append(
            {
                key
                + "Coordinate": {
                    "value": binding[key + "Value"],
                    "unit": binding[key + "Unit"],
                }
                for key in ["X", "Y", "Z"]
            }
        )

    return [{"Atoms": iri2data[iri]} for iri in iris]


def get_rotational_constants_data(kg_client: KgClient, iris: Sequence[str]):
    query = """PREFIX occ: <http://www.theworldavatar.com/ontology/ontocompchem/OntoCompChem.owl#>

SELECT *
WHERE {{
    VALUES ?RotationalConstants {{ {values} }}
    ?RotationalConstants
        occ:value ?Value ;
        occ:unit ?Unit .
}}""".format(
        values=" ".join("<{iri}>".format(iri=iri) for iri in iris)
    )

    res = kg_client.querySelect(query)
    _, bindings = flatten_sparql_select_response(res)

    iri2values: defaultdict[str, list] = defaultdict(list)
    iri2unit: dict[str, str] = dict()
    for binding in bindings:
        iri2values[binding["RotationalConstants"]].append(binding["Value"])
        iri2unit[binding["RotationalConstants"]] = binding["Unit"]

    return [{"values": iri2values[iri], "unit": iri2unit.get(iri)} for iri in iris]


def get_calculation_result_value_unit(kg_client: KgClient, iris: Sequence[str]):
    query = """PREFIX occ: <http://www.theworldavatar.com/ontology/ontocompchem/OntoCompChem.owl#>

SELECT *
WHERE {{
    VALUES ?CalculationResult {{ {values} }}
    ?CalculationResult occ:value ?Value ; occ:unit ?Unit .
}}""".format(
        values=" ".join("<{iri}>".format(iri=iri) for iri in iris)
    )

    res = kg_client.querySelect(query)
    _, bindings = flatten_sparql_select_response(res)

    iri2data = {
        binding["CalculationResult"]: {
            "value": binding["Value"],
            "unit": binding["Unit"],
        }
        for binding in bindings
    }

    return [iri2data.get(iri, {}) for iri in iris]


CALCULATION_RESULT_TYPE_TO_GETTER = defaultdict(
    lambda: get_calculation_result_value_unit,
    {
        URI_OCC + k: v
        for k, v in {
            "OptimizedGeometry": get_optimized_geometry_data,
            "RotationalConstants": get_rotational_constants_data,
        }.items()
    },
)


def get_calc_result_data(kg_client: KgClient, iris: Sequence[str]):
    query = """SELECT * 
WHERE {{
    VALUES ?CalculationResult {{ {values} }}
    ?CalculationResult a ?Type .
}}""".format(
        values=" ".join("<{iri}>".format(iri=iri) for iri in iris)
    )

    res = kg_client.querySelect(query)
    _, bindings = flatten_sparql_select_response(res)

    type2iris: defaultdict[str, list[dict]] = defaultdict(list)
    for binding in bindings:
        type2iris[binding["Type"]].append(binding["CalculationResult"])

    iri2data: dict[str, dict] = dict()
    for type, same_type_iris in type2iris.items():
        data = CALCULATION_RESULT_TYPE_TO_GETTER[type](
            kg_client=kg_client, iris=same_type_iris
        )
        iri2data.update({iri: datum for iri, datum in zip(same_type_iris, data)})

    return [iri2data.get(iri, {}) for iri in iris]


@cache
def get_ontocompchem_nodeDataRetriever(
    bg_client: Annotated[KgClient, Depends(get_ontocompchem_bgClient)]
):
    return NodeDataRetriever(
        kg_client=bg_client, type2getter={"occ:CalculationResult": get_calc_result_data}
    )
