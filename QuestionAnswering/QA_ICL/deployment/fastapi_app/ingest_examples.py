import json
import os
from typing import List

import numpy as np
from redis import Redis
from redis.commands.search.field import VectorField

from services.embed import IEmbedder, TritonEmbedder
from services.ingest import DataIngester
from services.redis import does_index_exist
from services.example_store.model import (
    EXAMPLES_INDEX_NAME,
    EXAMPLES_KEY_PREFIX,
    Nlq2ActionExample,
    Nlq2ActionExampleProcessed,
)


EXAMPLES_DIRNAME = "examples"
EXAMPLES_PROCESS_BATCHSIZE = 512
EXAMPLES_INSERT_BATCHSIZE = 512


def process_examples(embedder: IEmbedder, examples: List[Nlq2ActionExample]):
    embeddings = (
        embedder([example.nlq for example in examples]).astype(np.float32).tolist()
    )
    processed_examples = [
        Nlq2ActionExampleProcessed(
            nlq=example.nlq, action=example.action, nlq_embedding=embedding
        )
        for example, embedding in zip(examples, embeddings)
    ]
    return processed_examples


def transform_examples_preinsert(examples: List[Nlq2ActionExampleProcessed]):
    return [
        {
            "nlq": example.nlq,
            "action": example.action.model_dump_json(),
            "nlq_embedding": example.nlq_embedding,
        }
        for example in examples
    ]


def make_example_search_schema(vector_dim: int):
    return (
        VectorField(
            "$.nlq_embedding",
            "FLAT",
            {"TYPE": "FLOAT32", "DIM": vector_dim, "DISTANCE_METRIC": "IP"},
            as_name="vector",
        ),
    )


if __name__ == "__main__":
    redis_client = Redis(
        host=os.getenv("REDIS_HOST", "localhost"), decode_responses=True
    )

    if does_index_exist(redis_client=redis_client, index_name=EXAMPLES_INDEX_NAME):
        print(
            "Index {index_name} exists; examples have already been ingested.".format(
                index_name=EXAMPLES_INDEX_NAME
            )
        )
    else:
        print(
            "Index {index_name} does not exist.\n".format(
                index_name=EXAMPLES_INDEX_NAME
            )
        )

        embedder = TritonEmbedder(url=os.environ["TEXT_EMBEDDING_URL"])
        vector_dim = len(embedder(["warmup"])[0])
        print("Vector dimension: ", vector_dim)
        schema = make_example_search_schema(vector_dim=vector_dim)

        ingester = DataIngester(
            dirname=EXAMPLES_DIRNAME,
            unprocessed_type=Nlq2ActionExample,
            processed_type=Nlq2ActionExampleProcessed,
            process_func=lambda examples: process_examples(
                embedder=embedder, examples=examples
            ),
            process_batchsize=EXAMPLES_PROCESS_BATCHSIZE,
            redis_client=redis_client,
            redis_key_prefix=EXAMPLES_KEY_PREFIX,
            redis_preinsert_transform=transform_examples_preinsert,
            redis_insert_batchsize=EXAMPLES_INSERT_BATCHSIZE,
            redis_index_name=EXAMPLES_INDEX_NAME,
            redis_ft_schema=schema,
        )
        ingester.ingest()
        print("Ingestion complete.")
