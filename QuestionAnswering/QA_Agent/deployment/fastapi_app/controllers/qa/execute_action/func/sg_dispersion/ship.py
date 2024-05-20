from functools import cache
from typing import Annotated, Optional
from fastapi import Depends
from pydantic import BaseModel, Field

from controllers.kg import get_sgDispersion_bgClient
from services.entity_store import EntityStore, get_entity_store
from services.feature_info_client import FeatureInfoClient, get_featureInfoAgent_url
from services.kg import KgClient


class ShipMeta(BaseModel):
    mmsi: str = Field(..., alias="MMSI")
    maximum_static_draught: str = Field(..., alias="Maximum static draught")
    dimension: str = Field(..., alias="Dimension")
    imo_number: str = Field(..., alias="IMO number")
    ship_type: str = Field(..., alias="Ship type")
    call_sign: str = Field(..., alias="Call sign")


def get_ship_featureInfo_client(url: Annotated[str, Depends(get_featureInfoAgent_url)]):
    return FeatureInfoClient(url=url, entity_metadata_cls=ShipMeta)


class ShipLinker:
    def __init__(self, bg_client: KgClient, entity_store: EntityStore):
        self.bg_client = bg_client
        self.entity_store = entity_store

    def link(self, text: Optional[str], mmsi: Optional[str]):
        if mmsi:
            return self.link_mmsi(mmsi)
        return self.entity_store.link(surface_form=text, clsname="Ship")

    def link_mmsi(self, mmsi: str):
        query = """PREFIX disp: <https://www.theworldavatar.com/kg/ontodispersion/>
PREFIX om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>

SELECT ?IRI WHERE {{
  ?IRI a disp:Ship .
  ?IRI disp:hasProperty [ a disp:MMSI ; om:hasValue/om:hasNumericalValue {MMSI} ]
}}""".format(
            MMSI=mmsi
        )
        res = self.bg_client.query(query)

        return [row["IRI"]["value"] for row in res["results"]["bindings"]]


@cache
def get_ship_linker(
    bg_client: Annotated[KgClient, Depends(get_sgDispersion_bgClient)],
    entity_store: Annotated[EntityStore, Depends(get_entity_store)],
):
    return ShipLinker(bg_client=bg_client, entity_store=entity_store)
