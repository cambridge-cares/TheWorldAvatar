from dataclasses import asdict
from typing import Annotated

from fastapi import Depends
from redis import Redis

from services.core.embed import IEmbedder, get_embedder
from services.core.redis import get_redis_client
from services.core.retrieve_docs import DocsRetriever
from .model import LandUseTypeNode
from .land_use import LandUseTypeStore, get_landUseType_store


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
