from functools import cache
import json
import logging
from typing import Annotated

from fastapi import Depends
import numpy as np
from pydantic import TypeAdapter
from redis import Redis
from redis.commands.search.query import Query

from services.embed import IEmbedder, get_embedder
from services.example_store.model import (
    EXAMPLES_INDEX_NAME,
    DataRequest,
    Nlq2DataReqExample,
)
from services.redis import get_redis_client


logger = logging.getLogger(__name__)


class ExampleStore:
    def __init__(
        self,
        redis_client: Redis,
        embedder: IEmbedder,
    ):
        self.redis_client = redis_client
        self.embedder = embedder
        self.datareq_adapter = TypeAdapter(DataRequest)

    def retrieve_examples(self, nlq: str, k: int = 5):
        encoded_nlq = self.embedder([nlq])[0].astype(np.float32)
        knn_query = (
            Query("(*)=>[KNN {k} @vector $query_vector AS vector_score]".format(k=k))
            .sort_by("vector_score")
            .return_field("$.nlq", as_field="nlq")
            .return_field("$.data_req", as_field="data_req")
            .dialect(2)
        )
        res = self.redis_client.ft(EXAMPLES_INDEX_NAME).search(
            knn_query, {"query_vector": encoded_nlq.tobytes()}
        )

        return [
            Nlq2DataReqExample(
                nlq=doc.nlq, data_req=self.datareq_adapter.validate_json(doc.data_req)
            )
            for doc in res.docs
        ]


@cache
def get_example_store(
    redis_client: Annotated[Redis, Depends(get_redis_client)],
    embedder: Annotated[IEmbedder, Depends(get_embedder)],
):
    return ExampleStore(
        redis_client=redis_client,
        embedder=embedder,
    )
