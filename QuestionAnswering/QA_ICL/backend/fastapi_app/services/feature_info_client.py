from functools import cache
import os
from typing import Generic, List, Optional, Type, TypeVar
from pydantic import BaseModel

from services.requests import request_get_obj


EntityMetaT = TypeVar("EntityMetaT", bound=BaseModel)


class FeatureInfoTimeItem(BaseModel):
    id: str
    data: List[str]
    timeClass: str
    time: List[str]
    valuesClass: List[str]
    values: List[List[float]]
    units: List[str]


class FeatureInfoResponse(BaseModel, Generic[EntityMetaT]):
    meta: EntityMetaT
    time: Optional[List[FeatureInfoTimeItem]] = None


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
def get_featureInfoAgent_url():
    return os.getenv("ENDPOINT_FEATURE_INFO_AGENT")
