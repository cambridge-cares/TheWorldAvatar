from functools import cache
from typing import Annotated

from fastapi import Depends
from redis import Redis

from utils.str import camel_case_split
from core.redis import get_redis_client
from core.embed import IEmbedder, get_embedder
from core.align_enum import EnumAligner
from .model import PropertyUsage


@cache
def get_propertyUsage_aligner(
    embedder: Annotated[IEmbedder, Depends(get_embedder)],
    redis_client: Annotated[Redis, Depends(get_redis_client)],
):
    return EnumAligner(
        embedder=embedder,
        redis_client=redis_client,
        key="sg_buildings:property_usages",
        enum_cls=PropertyUsage,
        enum2label={
            x: " ".join(camel_case_split(x.value)).lower() for x in PropertyUsage
        },
    )
