from functools import cache
from typing import Annotated, Sequence

from fastapi import Depends

from services.kg import KgClient, get_ontozeolite_bgClient
from services.processs_response.augment_node import NodeDataRetriever


def get_zeolite_framework_data(kg_client: KgClient, iris: Sequence[str]):
    query = """PREFIX zeo: <http://www.theworldavatar.com/kg/ontozeolite/>

SELECT DISTINCT * WHERE {{
    VALUES ?Framework {{ {values} }}
    ?Framework zeo:hasFrameworkCode ?FrameworkCode .
}}""".format(
        values=" ".join("<{iri}>".format(iri=iri) for iri in iris)
    )

    _, bindings = kg_client.querySelectThenFlatten(query)

    iri2data = {
        binding["Framework"]: {"FrameworkCode": binding["FrameworkCode"]}
        for binding in bindings
    }
    return [iri2data.get(iri, {}) for iri in iris]


def get_zeolitic_material_data(kg_client: KgClient, iris: Sequence[str]):
    query = """PREFIX zeo: <http://www.theworldavatar.com/kg/ontozeolite/>

SELECT DISTINCT * WHERE {{
    VALUES ?Material {{ {values} }}
    ?Material zeo:hasChemicalFormula ?Formula .
}}""".format(
        values=" ".join("<{iri}>".format(iri=iri) for iri in iris)
    )

    _, bindings = kg_client.querySelectThenFlatten(query)

    iri2data = {
        binding["Material"]: {"ChemicalFormula": binding["Formula"]}
        for binding in bindings
    }
    return [iri2data.get(iri, {}) for iri in iris]


@cache
def get_ontozeolite_nodeDataRetriever(
    bg_client: Annotated[KgClient, Depends(get_ontozeolite_bgClient)]
):
    return NodeDataRetriever(
        kg_client=bg_client,
        type2getter={
            "zeo:ZeoliteFramework": get_zeolite_framework_data,
            "zeo:ZeoliticMaterial": get_zeolitic_material_data
        },
    )
