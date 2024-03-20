from typing import Annotated
from fastapi import Depends
from redis import Redis


from services.core.embed import IEmbedder, get_embedder
from services.core.redis import get_redis_client
from services.core.align_enum import EnumAligner
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
