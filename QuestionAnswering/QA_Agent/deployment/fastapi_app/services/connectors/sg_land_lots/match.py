from dataclasses import asdict
from functools import cache
from typing import Annotated

from fastapi import Depends
from redis import Redis
from pydantic.dataclasses import dataclass

from .land_use import LandUseTypeStore, get_landUseType_store
from services.core.embed import IEmbedder, get_embedder
from services.core.redis import get_redis_client
from services.core.kg import KgClient
from services.core.retrieve_docs import DocsRetriever
from .kg import get_sgLandLots_bgClient
from .model import LandUseTypeNode



def _linearize_landUseType(datum: LandUseTypeNode):
    return "category: {category}; label: {label}; comment: {comment}.".format(
        category=datum.clsname, label=datum.label, comment=datum.comment
    )


def get_landUseType_retriever(
    embedder: Annotated[IEmbedder, Depends(get_embedder)],
    redis_client: Annotated[Redis, Depends(get_redis_client)],
    land_use_type_store: Annotated[LandUseTypeStore, Depends(get_landUseType_store)],
):
    return DocsRetriever(
        embedder=embedder,
        redis_client=redis_client,
        key="sg_land_lots:land_use_types",
        docs=land_use_type_store.get_all(),
        linearize=_linearize_landUseType,
        jsonify=asdict,
    )
