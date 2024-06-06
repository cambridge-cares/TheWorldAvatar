import numpy as np
from redis import Redis
from redis.commands.search.field import VectorField

from services.embed import IEmbedder, TritonEmbedder
from services.ingest import DataIngester, IngestArgs, load_ingest_args
from services.redis import does_index_exist
from model.nlq2req import (
    EXAMPLES_INDEX_NAME,
    EXAMPLES_KEY_PREFIX,
    Nlq2DataReqExample,
    Nlq2DataReqExampleProcessed,
)


EXAMPLES_DIRNAME = "examples"
EXAMPLES_PROCESS_BATCHSIZE = 512
EXAMPLES_INSERT_BATCHSIZE = 512


def process_examples(embedder: IEmbedder, examples: list[Nlq2DataReqExample]):
    embeddings = (
        embedder([example.nlq for example in examples]).astype(np.float32).tolist()
    )
    processed_examples = [
        Nlq2DataReqExampleProcessed(
            nlq=example.nlq, data_req=example.data_req, nlq_embedding=embedding
        )
        for example, embedding in zip(examples, embeddings)
    ]
    return processed_examples


def transform_examples_preinsert(examples: list[Nlq2DataReqExampleProcessed]):
    return [
        {
            "nlq": example.nlq,
            "data_req": example.data_req.model_dump_json(),
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


def main(args: IngestArgs):
    redis_client = Redis(host=args.redis_host, decode_responses=True)

    if not args.drop_index and does_index_exist(
        redis_client=redis_client, index_name=EXAMPLES_INDEX_NAME
    ):
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

        embedder = TritonEmbedder(url=args.text_embedding_url)
        vector_dim = len(embedder(["warmup"])[0])
        print("Vector dimension: ", vector_dim)
        schema = make_example_search_schema(vector_dim=vector_dim)

        ingester = DataIngester(
            dirname=EXAMPLES_DIRNAME,
            invalidate_cache=args.invalidate_cache,
            unprocessed_type=Nlq2DataReqExample,
            processed_type=Nlq2DataReqExampleProcessed,
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


if __name__ == "__main__":
    args = load_ingest_args()
    main(args)
