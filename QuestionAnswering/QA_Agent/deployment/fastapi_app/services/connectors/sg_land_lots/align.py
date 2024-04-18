from typing import Annotated

from fastapi import Depends
from redis import Redis

from services.core.embed import IEmbedder, get_embedder
from services.core.redis import get_redis_client
from services.core.align_enum import BiEnumAligner
from .model import PlotCatAttrKey, PlotNumAttrKey


def get_plotAttrKey_aligner(
    embedder: Annotated[IEmbedder, Depends(get_embedder)],
    redis_client: Annotated[Redis, Depends(get_redis_client)],
):
    return BiEnumAligner(
        embedder=embedder,
        redis_client=redis_client,
        key="sg_land_lots:plot_attr_keys",
        enum_clses=(PlotCatAttrKey, PlotNumAttrKey),
    )
