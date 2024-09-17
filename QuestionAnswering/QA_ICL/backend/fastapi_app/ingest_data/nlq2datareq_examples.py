import numpy as np
from redis import Redis
from redis.commands.search.field import VectorField

from model.nlq2datareq import (
    NLQ2DATAREQ_EXAMPLES_INDEX_NAME,
    NLQ2DATAREQ_EXAMPLES_KEY_PREFIX,
    Nlq2DataReqExample,
    Nlq2DataReqExampleProcessed,
)
from services.embed import IEmbedder, TritonEmbedder
from services.ingest import (
    DataIngester,
    InsertThenIndexArgs,
    load_insert_then_index_args,
)
from services.redis import get_index_existence


NLQ2DATAREQ_EXAMPLES_DIRNAME = "nlq2datareq_examples"
NLQ2DATAREQ_EXAMPLES_PROCESS_BATCHSIZE = 512
NLQ2DATAREQ_EXAMPLES_INSERT_BATCHSIZE = 512


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
            "data_req": example.data_req.model_dump_json(
                exclude_unset=True, exclude_none=True
            ),
            "nlq_embedding": example.nlq_embedding,
        }
        for example in examples
    ]


def make_example_search_schema(vector_dim: int):
    return (
        VectorField(
            "$.nlq_embedding",
            "FLAT",
            {"TYPE": "FLOAT32", "DIM": vector_dim, "DISTANCE_METRIC": "COSINE"},
            as_name="vector",
        ),
    )


def main(args: InsertThenIndexArgs):
    redis_client = Redis(host=args.redis_host, decode_responses=True)
    does_index_exist = get_index_existence(
        redis_client=redis_client, index_name=NLQ2DATAREQ_EXAMPLES_INDEX_NAME
    )

    if args.drop_index:
        if does_index_exist:
            print(
                "Index {index_name} exists but will be recreated.\n".format(
                    index_name=NLQ2DATAREQ_EXAMPLES_INDEX_NAME
                )
            )
            redis_client.ft(NLQ2DATAREQ_EXAMPLES_INDEX_NAME).dropindex(
                delete_documents=True
            )
    elif does_index_exist:
        print(
            "Index {index_name} exists; examples have already been ingested.".format(
                index_name=NLQ2DATAREQ_EXAMPLES_INDEX_NAME
            )
        )
        return
    else:
        print(
            "Index {index_name} does not exist.\n".format(
                index_name=NLQ2DATAREQ_EXAMPLES_INDEX_NAME
            )
        )

    embedder = TritonEmbedder(url=args.text_embedding_url)
    vector_dim = len(embedder(["warmup"])[0])
    print("Vector dimension: ", vector_dim)
    schema = make_example_search_schema(vector_dim=vector_dim)

    ingester = DataIngester(
        dirname=NLQ2DATAREQ_EXAMPLES_DIRNAME,
        invalidate_cache=args.invalidate_cache,
        unprocessed_type=Nlq2DataReqExample,
        processed_type=Nlq2DataReqExampleProcessed,
        process_func=lambda examples: process_examples(
            embedder=embedder, examples=examples
        ),
        process_batchsize=NLQ2DATAREQ_EXAMPLES_PROCESS_BATCHSIZE,
        redis_client=redis_client,
        redis_key_prefix=NLQ2DATAREQ_EXAMPLES_KEY_PREFIX,
        redis_preinsert_transform=transform_examples_preinsert,
        redis_insert_batchsize=NLQ2DATAREQ_EXAMPLES_INSERT_BATCHSIZE,
        redis_index_name=NLQ2DATAREQ_EXAMPLES_INDEX_NAME,
        redis_ft_schema=schema,
    )
    ingester.ingest()
    print("Ingestion complete.")


if __name__ == "__main__":
    args = load_insert_then_index_args()
    main(args)
