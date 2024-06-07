from typing import Annotated, Optional

from fastapi import Depends
from services.stores.entity_store.base import IEntityLinker
from services.kg import KgClient, get_ontozeolite_bgClient


class ZeoliteFrameworkLinker(IEntityLinker):
    def __init__(self, bg_client: KgClient):
        self.bg_client = bg_client

    def link(self, text: Optional[str], **kwargs):
        if "framework_code" not in kwargs:
            return []

        query = """PREFIX zeo: <http://www.theworldavatar.com/kg/ontozeolite/>
SELECT ?Framework
WHERE {{
    ?Framework zeo:hasFrameworkCode "{framework_code}" .
}}""".format(
            framework_code=kwargs["framework_code"]
        )

        _, bindings = self.bg_client.querySelectThenFlatten(query)
        return [row["Framework"] for row in bindings]


def get_zeoliteFramework_linker(
    bg_client: Annotated[KgClient, Depends(get_ontozeolite_bgClient)]
):
    return ZeoliteFrameworkLinker(bg_client=bg_client)


class ZeoliticMaterialLinker(IEntityLinker):
    def __init__(self, bg_client: KgClient):
        self.bg_client = bg_client

    def link(self, text: Optional[str], **kwargs):
        if "formula" not in kwargs:
            return []

        query = """PREFIX zeo: <http://www.theworldavatar.com/kg/ontozeolite/>
SELECT ?Material
WHERE {{
    ?Material zeo:hasChemicalFormula "{formula}" .
}}""".format(
            formula=kwargs["formula"]
        )

        _, bindings = self.bg_client.querySelectThenFlatten(query)
        return [row["Material"] for row in bindings]


def get_zeoliticMaterial_linker(
    bg_client: Annotated[KgClient, Depends(get_ontozeolite_bgClient)]
):
    return ZeoliticMaterialLinker(bg_client=bg_client)
