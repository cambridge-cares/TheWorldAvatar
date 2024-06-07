from functools import cache
from typing import Annotated, Optional

from fastapi import Depends

from services.kg import get_sgDispersion_bgClient
from services.stores.entity_store.base import IEntityLinker
from services.kg import KgClient


class ShipLinker(IEntityLinker):
    def __init__(self, bg_client: KgClient):
        self.bg_client = bg_client

    def link(self, text: Optional[str], **kwargs):
        if "mmsi" not in kwargs:
            return []

        query = """PREFIX disp: <https://www.theworldavatar.com/kg/ontodispersion/>
PREFIX om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>

SELECT ?IRI WHERE {{
  ?IRI a disp:Ship .
  ?IRI disp:hasProperty [ a disp:MMSI ; om:hasValue/om:hasNumericalValue {MMSI} ]
}}""".format(
            MMSI=kwargs["mmsi"]
        )
        _, bindings  = self.bg_client.querySelectThenFlatten(query)

        return [binding["IRI"] for binding in bindings]


@cache
def get_ship_linker(bg_client: Annotated[KgClient, Depends(get_sgDispersion_bgClient)]):
    return ShipLinker(bg_client=bg_client)
