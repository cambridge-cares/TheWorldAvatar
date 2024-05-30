from collections import defaultdict
from functools import cache
from typing import Annotated, DefaultDict, Dict, List, Sequence

from fastapi import Depends
from services.kg import KgClient, get_ontocompchem_bgClient
from services.processs_response.augment_node import NodeDataRetriever
from utils.rdf import flatten_sparql_select_response


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

    iri2data: DefaultDict[str, List[dict]] = defaultdict(list)
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

    iri2values: DefaultDict[str, list] = defaultdict(list)
    iri2unit: Dict[str, str] = dict()
    for binding in bindings:
        iri2values[binding["RotationalConstants"]].append(binding["Value"])
        iri2unit[binding["RotationalConstants"]] = binding["Unit"]

    return [{"values": iri2values[iri], "unit": iri2unit.get(iri)} for iri in iris]


def get_molcomp_value_unit(kg_client: KgClient, iris: Sequence[str]):
    query = """PREFIX occ: <http://www.theworldavatar.com/ontology/ontocompchem/OntoCompChem.owl#>

SELECT *
WHERE {{
    VALUES ?MolComp {{ {values} }}
    ?MolComp occ:value ?Value ; occ:unit ?Unit .
}}""".format(
        values=" ".join("<{iri}>".format(iri=iri) for iri in iris)
    )

    res = kg_client.querySelect(query)
    _, bindings = flatten_sparql_select_response(res)

    iri2data = {
        binding["MolComp"]: {"value": binding["Value"], "unit": binding["Unit"]}
        for binding in bindings
    }

    return [iri2data.get(iri, {}) for iri in iris]


ONTOCOMPCHEM_TYPE2GETTER = {
    "OptimizedGeometry": get_optimized_geometry_data,
    "RotationalConstants": get_rotational_constants_data,
    **{
        key: get_molcomp_value_unit
        for key in [
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
        ]
    },
}


@cache
def get_ontocompchem_nodeDataRetriever(
    bg_client: Annotated[KgClient, Depends(get_ontocompchem_bgClient)]
):
    return NodeDataRetriever(kg_client=bg_client, type2getter=ONTOCOMPCHEM_TYPE2GETTER)
