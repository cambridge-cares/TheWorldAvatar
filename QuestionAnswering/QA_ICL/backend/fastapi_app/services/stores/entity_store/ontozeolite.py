from typing import Annotated

from fastapi import Depends
from services.stores.entity_store.base import IEntityLinker
from services.sparql import SparqlClient, get_ontozeolite_endpoint


class ZeoliteFrameworkLinker(IEntityLinker):
    def __init__(self, ontozeolite_endpoint: str):
        self.sparql_client = SparqlClient(ontozeolite_endpoint)

    def link(self, text: str | None, **kwargs):
        if "code" not in kwargs:
            return []

        query = f"""PREFIX zeo: <http://www.theworldavatar.com/kg/ontozeolite/>
SELECT ?Framework
WHERE {{
    ?Framework zeo:hasFrameworkCode "{kwargs["code"]}" .
}}"""

        _, bindings = self.sparql_client.querySelectThenFlatten(query)
        return [row["Framework"] for row in bindings]


def get_zeoliteFramework_linker(
    endpoint: Annotated[str, Depends(get_ontozeolite_endpoint)]
):
    return ZeoliteFrameworkLinker(ontozeolite_endpoint=endpoint)


class ZeoliticMaterialLinker(IEntityLinker):
    def __init__(self, ontozeolite_endpoint: str):
        self.sparql_client = SparqlClient(ontozeolite_endpoint)

    def link(self, text: str | None, **kwargs):
        if "formula" not in kwargs:
            return []

        query = """PREFIX zeo: <http://www.theworldavatar.com/kg/ontozeolite/>
SELECT ?Material
WHERE {{
    ?Material zeo:hasChemicalFormula "{formula}" .
}}""".format(
            formula=kwargs["formula"]
        )

        _, bindings = self.sparql_client.querySelectThenFlatten(query)
        return [row["Material"] for row in bindings]


def get_zeoliticMaterial_linker(
    endpoint: Annotated[str, Depends(get_ontozeolite_endpoint)]
):
    return ZeoliticMaterialLinker(ontozeolite_endpoint=endpoint)
