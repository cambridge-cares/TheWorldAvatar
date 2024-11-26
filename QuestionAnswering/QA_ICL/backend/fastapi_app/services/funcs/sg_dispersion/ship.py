from typing import Annotated
from fastapi import Depends
from pydantic import BaseModel, Field

from services.feature_info_client import FeatureInfoClient, get_featureInfoAgent_url


class ShipMeta(BaseModel):
    mmsi: str = Field(..., alias="MMSI")
    maximum_static_draught: str | None = Field(default=None, alias="Maximum static draught")
    energy_efficiency: str | None = Field(default=None, alias="Energy efficiency")
    specific_fuel_consumption: str | None = Field(default=None, alias="Specific fuel consumption")
    dimension: str | None = Field(default=None, alias="Dimension") 
    specific_co2_emission: str | None = Field(default=None, alias="Specific CO2 emission")
    imo_number: str | None = Field(default=None, alias="IMO number")
    ship_type: str | None = Field(default=None, alias="Ship type")
    call_sign: str | None = Field(default=None, alias="Call sign")


def get_ship_featureInfo_client(url: Annotated[str, Depends(get_featureInfoAgent_url)]):
    return FeatureInfoClient(url=url, entity_metadata_cls=ShipMeta)
