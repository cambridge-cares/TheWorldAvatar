from typing import Annotated
from fastapi import Depends
from redis import Redis

from services.align_enum import EnumAligner
from services.embed import IEmbedder, get_embedder
from services.redis_client import get_redis_client
from .constants import FactoryAttrKey, FactoryConcept


def get_factory_attr_key_aligner(
    embedder: Annotated[IEmbedder, Depends(get_embedder)],
    redis_client: Annotated[Redis, Depends(get_redis_client)],
):
    return EnumAligner(
        embedder=embedder,
        redis_client=redis_client,
        key="sg_factories:factory_attr_keys",
        enum_cls=FactoryAttrKey,
    )


def get_factory_concept_aligner(
    embedder: Annotated[IEmbedder, Depends(get_embedder)],
    redis_client: Annotated[Redis, Depends(get_redis_client)],
):
    return EnumAligner(
        embedder=embedder,
        redis_client=redis_client,
        key="sg_factories:factory_concepts",
        enum_cls=FactoryConcept,
    )
