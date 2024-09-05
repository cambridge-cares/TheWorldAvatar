from typing import Annotated
from fastapi import Depends
from pydantic import BaseModel, Field

from services.feature_info_client import FeatureInfoClient, get_featureInfoAgent_url


class ShipMeta(BaseModel):
    mmsi: str = Field(..., alias="MMSI")
    maximum_static_draught: str = Field(..., alias="Maximum static draught")
    dimension: str = Field(..., alias="Dimension")
    imo_number: str = Field(..., alias="IMO number")
    ship_type: str = Field(..., alias="Ship type")
    call_sign: str = Field(..., alias="Call sign")


def get_ship_featureInfo_client(url: Annotated[str, Depends(get_featureInfoAgent_url)]):
    return FeatureInfoClient(url=url, entity_metadata_cls=ShipMeta)
