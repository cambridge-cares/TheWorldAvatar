
from typing import Annotated

from fastapi import Depends
from redis import Redis

from services.connectors.sg_data_centres.model import DataCentreAttrKey
from services.core.align_enum import EnumAligner
from services.core.embed import IEmbedder, get_embedder
from services.core.redis import get_redis_client


def get_dataCentreAttrkey_aligner(
    embedder: Annotated[IEmbedder, Depends(get_embedder)],
    redis_client: Annotated[Redis, Depends(get_redis_client)],
):
    return EnumAligner(
        embedder=embedder,
        redis_client=redis_client,
        key="sg_factories:factory_num_attr_keys",
        enum_cls=DataCentreAttrKey,
    )