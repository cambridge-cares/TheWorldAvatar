import os
from typing import Annotated
from fastapi import Depends
from pydantic import BaseModel, Field

from services.feature_info_client import FeatureInfoClient, get_featureInfoAgent_url


class CarparkMeta(BaseModel):
    name: str = Field(..., alias="Carpark name")


class CarparkFeatureInfoClient(FeatureInfoClient[CarparkMeta]):
    def __init__(self, url: str, sg_stack_internal_carpark_endpoint: str):
        super().__init__(url=url, entity_metadata_cls=CarparkMeta)
        self.sg_stack_internal_carpark_endpoint = sg_stack_internal_carpark_endpoint

    def query(self, **kwargs):
        return super().query(endpoint=self.sg_stack_internal_carpark_endpoint, **kwargs)


def get_carpark_featureInfo_client(
    url: Annotated[str, Depends(get_featureInfoAgent_url)]
):
    return CarparkFeatureInfoClient(
        url=url,
        sg_stack_internal_carpark_endpoint=os.environ[
            "SG_STACK_INTERNAL_CARPARK_ENDPOINT"
        ],
    )
