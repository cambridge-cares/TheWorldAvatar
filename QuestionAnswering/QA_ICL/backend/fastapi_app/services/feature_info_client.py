from functools import cache
import os
from typing import Annotated, Generic, Type, TypeVar

from fastapi import Depends
from config import AppSettings, get_app_settings
from pydantic import BaseModel

from services.requests import request_get_obj


EntityMetaT = TypeVar("EntityMetaT", bound=BaseModel)


class FeatureInfoTimeItem(BaseModel):
    id: str
    data: list[str]
    timeClass: str
    time: list[str]
    valuesClass: list[str]
    values: list[list[float]]
    units: list[str]


class FeatureInfoResponse(BaseModel, Generic[EntityMetaT]):
    meta: EntityMetaT
    time: list[FeatureInfoTimeItem] | None = None


class FeatureInfoClient(Generic[EntityMetaT]):
    def __init__(self, url: str, entity_metadata_cls: Type[EntityMetaT]):
        self.url = url
        self.entity_meta_type = entity_metadata_cls

    def query(self, **kwargs):
        return request_get_obj(
            self.url,
            params=kwargs,
            response_type=FeatureInfoResponse[self.entity_meta_type],
        )


@cache
def get_featureInfoAgent_url(app_settings: Annotated[AppSettings, Depends(get_app_settings)]):
    return app_settings.singapore_endpoints.feature_info_agent
