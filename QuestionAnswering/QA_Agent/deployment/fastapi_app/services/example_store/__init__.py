from functools import cache
import json
import logging
from typing import Annotated

from fastapi import Depends
import numpy as np
from redis import Redis
from redis.commands.search.query import Query

from services.embed import IEmbedder, get_embedder
from services.example_store.model import Nlq2ActionExample
from services.redis import get_redis_client


logger = logging.getLogger(__name__)


class ExampleStore:
    EXAMPLES_KEY_PREFIX = "nlq2actionExamples:"
    EXAMPLES_INDEX_NAME = "idx:nlq2actionExamples_vss"

    def __init__(
        self,
        redis_client: Redis,
        embedder: IEmbedder,
    ):
        self.redis_client = redis_client
        self.embedder = embedder

    def retrieve_examples(self, qa_domain: str, nlq: str, k: int = 5):
        encoded_nlq = self.embedder([nlq])[0].astype(np.float32)
        knn_query = (
            Query(
                "(@qa_domain:{{{qa_domain}}})=>[KNN {k} @vector $query_vector AS vector_score]".format(
                    qa_domain=qa_domain, k=k
                )
            )
            .sort_by("vector_score")
            .return_field("$.qa_domain", as_field="qa_domain")
            .return_field("$.nlq", as_field="nlq")
            .return_field("$.action", as_field="action")
            .dialect(2)
        )
        res = self.redis_client.ft(self.EXAMPLES_INDEX_NAME).search(
            knn_query, {"query_vector": encoded_nlq.tobytes()}
        )

        return [
            Nlq2ActionExample(
                qa_domain=doc.qa_domain, nlq=doc.nlq, action=json.loads(doc.action)
            )
            for doc in res.docs
        ]

    def retrieve_schema(self, nlq: str, k: int = 10):
        pass


@cache
def get_nlq2action_retriever(
    redis_client: Annotated[Redis, Depends(get_redis_client)],
    embedder: Annotated[IEmbedder, Depends(get_embedder)],
):
    return ExampleStore(
        redis_client=redis_client,
        embedder=embedder,
    )
