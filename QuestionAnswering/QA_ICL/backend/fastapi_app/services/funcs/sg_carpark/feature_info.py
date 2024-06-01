from typing import Annotated
from fastapi import Depends
from pydantic import BaseModel, Field

from config import AppSettings, get_app_settings
from services.feature_info_client import FeatureInfoClient, get_featureInfoAgent_url


class CarparkMeta(BaseModel):
    name: str = Field(..., alias="Carpark name")


class CarparkFeatureInfoClient(FeatureInfoClient[CarparkMeta]):
    def __init__(self, url: str, endpoint_singapore_carpark_internal: str):
        super().__init__(url=url, entity_metadata_cls=CarparkMeta)
        self.endpoint_singapore_carpark_internal = endpoint_singapore_carpark_internal

    def query(self, **kwargs):
        return super().query(
            endpoint=self.endpoint_singapore_carpark_internal, **kwargs
        )


def get_carpark_featureInfo_client(
    url: Annotated[str, Depends(get_featureInfoAgent_url)],
    settings: Annotated[AppSettings, Depends(get_app_settings)],
):
    return CarparkFeatureInfoClient(
        url=url,
        endpoint_singapore_carpark_internal=settings.singapore_endpoints.carpark_internal,
    )
