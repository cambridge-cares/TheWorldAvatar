from functools import cache
import logging
from typing import Annotated

from fastapi import Depends
import numpy as np
from redis import Redis
from redis.commands.search.query import Query

from services.embed import IEmbedder, get_embedder
from services.redis import get_redis_client
from .model import RELATIONS_INDEX_NAME, RDFRelation


logger = logging.getLogger(__name__)


class SchemaStore:
    def __init__(
        self,
        redis_client: Redis,
        embedder: IEmbedder,
    ):
        self.redis_client = redis_client
        self.embedder = embedder

    def retrieve_relations(self, nlq: str, k: int = 5):
        encoded_nlq = self.embedder([nlq])[0].astype(np.float32)
        knn_query = (
            Query("(*)=>[KNN {k} @vector $query_vector AS vector_score]".format(k=k))
            .sort_by("vector_score")
            .return_field("$.s", as_field="s")
            .return_field("$.p", as_field="p")
            .return_field("$.o", as_field="o")
            .dialect(2)
        )
        res = self.redis_client.ft(RELATIONS_INDEX_NAME).search(
            knn_query, {"query_vector": encoded_nlq.tobytes()}
        )

        return [RDFRelation(s=doc.s, p=doc.p, o=doc.o) for doc in res.docs]


@cache
def get_schema_store(
    redis_client: Annotated[Redis, Depends(get_redis_client)],
    embedder: Annotated[IEmbedder, Depends(get_embedder)],
):
    return SchemaStore(
        redis_client=redis_client,
        embedder=embedder,
    )
