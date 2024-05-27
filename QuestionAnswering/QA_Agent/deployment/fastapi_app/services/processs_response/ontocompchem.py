from functools import cache
from typing import Annotated

from fastapi import Depends
from constants.prefixes import URI_OCC
from services.kg import KgClient, get_ontocompchem_bgClient
from services.processs_response.expand_response import SparqlResponseExpander
from services.processs_response.utils import iri_slot_template_to_func
from utils.rdf import flatten_sparql_response


def get_optimized_geometry_data(iri: str, kg_client: KgClient):
    query = """PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX occ: <http://www.theworldavatar.com/ontology/ontocompchem/OntoCompChem.owl#>

SELECT *
WHERE {{
    VALUES ?OptimizedGeometry {{ <{IRI}> }}
    ?OptimizedGeometry ^os:fromGeometry ?X, ?Y, ?Z .
    ?Atom 
        os:hasXCoordinate ?X ;
        os:hasYCoordinate ?Y ;
        os:hasZCoordinate ?Z .
    ?X os:value ?XValue ; os:unit ?XUnit .
    ?Y os:value ?YValue ; os:unit ?YUnit .
    ?Z os:value ?ZValue ; os:unit ?ZUnit .
}}""".format(
        IRI=iri
    )

    res = kg_client.query(query)
    _, bindings = flatten_sparql_response(res)

    data = {
        "Atoms": [
            {
                key
                + "Coordinate": {
                    "value": binding[key + "Value"],
                    "unit": binding[key + "Unit"],
                }
                for key in ["X", "Y", "Z"]
            }
            for binding in bindings
        ]
    }

    return {k: v for k, v in data.items() if v}


def get_rotational_constants_data(iri: str, kg_client: KgClient):
    query = """PREFIX occ: <http://www.theworldavatar.com/ontology/ontocompchem/OntoCompChem.owl#>

SELECT *
WHERE {{
    VALUES ?RotationalConstants {{ <{IRI}> }}
    ?RotationalConstants
        occ:value ?Value ;
        occ:unit ?Unit .
}}""".format(
        IRI=iri
    )

    res = kg_client.query(query)
    _, bindings = flatten_sparql_response(res)

    data = {
        "values": [binding["Value"] for binding in bindings],
        "unit": bindings[0]["Unit"] if bindings else None,
    }

    return {k: v for k, v in data.items() if v}


def get_molcomp_value_unit(iri: str, kg_client: KgClient):
    query = """PREFIX occ: <http://www.theworldavatar.com/ontology/ontocompchem/OntoCompChem.owl#>

SELECT *
WHERE {{
    VALUES ?MolComp {{ <{IRI}> }}
    ?MolComp occ:value ?Value ; occ:unit ?Unit .
}}""".format(
        IRI=iri
    )

    res = kg_client.query(query)
    _, bindings = flatten_sparql_response(res)

    if not bindings:
        return {}

    binding = bindings[0]
    return {"value": binding["Value"], "unit": binding["Unit"]}


ONTOCOMPCHEM_TYPE2GETTER = {
    URI_OCC + "OptimizedGeometry": get_optimized_geometry_data,
    URI_OCC + "RotationalConstants": get_rotational_constants_data,
    **{
        URI_OCC + key: get_molcomp_value_unit
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
def get_ontocompchem_responseExpander(
    bg_client: Annotated[KgClient, Depends(get_ontocompchem_bgClient)]
):
    return SparqlResponseExpander(
        kg_client=bg_client, type2getter=ONTOCOMPCHEM_TYPE2GETTER
    )
