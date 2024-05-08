from importlib.resources import files
import json
import logging
from typing import Annotated, Iterable

from fastapi import Depends
import numpy as np
from redis import Redis
from redis.commands.search.field import VectorField
from redis.commands.search.indexDefinition import IndexDefinition, IndexType
from redis.commands.search.query import Query
from pydantic.dataclasses import dataclass

from utils.itertools_recipes import batched
from services.embed import IEmbedder, get_embedder
from services.redis import does_index_exist, get_redis_client


@dataclass
class Nlq2ActionExample:
    nlq: str
    action: dict


logger = logging.getLogger(__name__)


class Nlq2ActionRetriever:
    EXAMPLES_KEY_PREFIX = "nlq2actionExamples:"
    EXAMPLES_INDEX_NAME = "idx:nlq2actionExamples_vss"

    @classmethod
    def _insert_examples_and_create_index(
        cls,
        redis_client: Redis,
        embedder: IEmbedder,
        examples: Iterable[Nlq2ActionExample],
    ):

        offset = 0
        vector_dim = None

        logger.info("Inserting examples into Redis...")

        for chunk in batched(examples, 10):
            chunk = list(chunk)
            embeddings = (
                embedder([example.nlq for example in chunk]).astype(np.float32).tolist()
            )
            if vector_dim is None:
                vector_dim = len(embeddings[0])

            pipeline = redis_client.pipeline()
            for i, (example, embedding) in enumerate(zip(chunk, embeddings)):
                redis_key = cls.EXAMPLES_KEY_PREFIX + str(offset + i)
                doc = dict(
                    nlq=example.nlq,
                    action=json.dumps(example.action),
                    nlq_embedding=embedding,
                )
                pipeline.json().set(redis_key, "$", doc)
            pipeline.execute()

            offset += len(chunk)

        logger.info("Insertion done")

        if vector_dim is None:
            raise ValueError(
                "Index for natural language question-to-action examples is not found and must be created. Therefore, `examples` must not be None."
            )

        logger.info("Creating index for examples...")

        schema = (
            VectorField(
                "$.nlq_embedding",
                "FLAT",
                {"TYPE": "FLOAT32", "DIM": vector_dim, "DISTANCE_METRIC": "IP"},
                as_name="vector",
            ),
        )
        definition = IndexDefinition(
            prefix=[cls.EXAMPLES_KEY_PREFIX], index_type=IndexType.JSON
        )
        redis_client.ft(cls.EXAMPLES_INDEX_NAME).create_index(
            fields=schema, definition=definition
        )

        logger.info("Index {index} created".format(index=cls.EXAMPLES_INDEX_NAME))

    def __init__(
        self,
        redis_client: Redis,
        embedder: IEmbedder,
        examples: Iterable[Nlq2ActionExample],
    ):
        if not does_index_exist(redis_client, self.EXAMPLES_INDEX_NAME):
            logger.info(
                "Index for examples {index} not found".format(
                    index=self.EXAMPLES_INDEX_NAME
                )
            )
            self._insert_examples_and_create_index(
                redis_client=redis_client, embedder=embedder, examples=examples
            )

        self.redis_client = redis_client
        self.embedder = embedder

    def retrieve_examples(self, nlq: str, k: int = 5):
        encoded_nlq = self.embedder([nlq])[0].astype(np.float32)
        knn_query = (
            Query("(*)=>[KNN {k} @vector $query_vector AS vector_score]".format(k=k))
            .sort_by("vector_score")
            .return_field("$.nlq", as_field="nlq")
            .return_field("$.action", as_field="action")
            .dialect(2)
        )
        res = self.redis_client.ft(self.EXAMPLES_INDEX_NAME).search(
            knn_query, {"query_vector": encoded_nlq.tobytes()}
        )

        return [
            Nlq2ActionExample(nlq=doc.nlq, action=json.loads(doc.action))
            for doc in res.docs
        ]

    def retrieve_schema(self, nlq: str, k: int = 10):
        pass


def gen_nlq2action_examples(domain: str):
    for file in files("resources.examples." + domain).iterdir():
        if file.is_file() and file.name.lower().endswith("_examples.json"):
            for example in json.loads(file.read_text()):
                yield Nlq2ActionExample(nlq=example["input"], action=example["output"])


def get_sg_nlq2action_retriever(
    redis_client: Annotated[Redis, Depends(get_redis_client)],
    embedder: Annotated[IEmbedder, Depends(get_embedder)],
):
    return Nlq2ActionRetriever(
        redis_client=redis_client,
        embedder=embedder,
        examples=gen_nlq2action_examples("singapore"),
    )
