from typing import Annotated, List, Optional
from fastapi import Depends
from pydantic import BaseModel, Field

from services.feature_info_client import FeatureInfoClient, get_featureInfoAgentUrl


class ShipFeatureInfoMeta(BaseModel):
    mmsi: str = Field(..., alias="MMSI")
    maximum_static_draught: str = Field(..., alias="Maximum static draught")
    dimension: str = Field(..., alias="Dimension")
    imo_number: str = Field(..., alias="IMO number")
    ship_type: str = Field(..., alias="Ship type")
    call_sign: str = Field(..., alias="Call sign")


class ShipFeatureInfoTimeItem(BaseModel):
    id: str
    data: List[str]
    timeClass: str
    time: List[str]
    valuesClass: List[str]
    values: List[List[float]]
    units: List[str]


class ShipFeatureInfo(BaseModel):
    meta: ShipFeatureInfoMeta
    time: Optional[List[ShipFeatureInfoTimeItem]] = None


def get_ship_featureInfoClient(url: Annotated[str, Depends(get_featureInfoAgentUrl)]):
    return FeatureInfoClient(url=url, type=ShipFeatureInfo)
