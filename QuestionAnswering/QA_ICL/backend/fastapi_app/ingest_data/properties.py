import numpy as np
from redis import Redis
from redis.commands.search.field import VectorField

from services.embed import IEmbedder, TritonEmbedder
from services.ingest import (
    DataIngester,
    InsertThenIndexArgs,
    load_insert_then_index_args,
)
from services.redis import get_index_existence
from model.rdf_schema import (
    PROPERTIES_INDEX_NAME,
    PROPERTIES_KEY_PREFIX,
    RDFProperty,
    RDFPropertyProcessed,
)
from utils.rdf import extract_name


PROPERTIES_DIRNAME = "schema/properties"
PROPERTIES_PROCESS_BATCHSIZE = 512
PROPERTIES_INSERT_BATCHSIZE = 512


def process_properties(
    embedder: IEmbedder,
    properties: list[RDFProperty],
):
    texts = [
        "\n".join(
            x
            for x in [
                "name: " + extract_name(entry.iri),
                "label: " + entry.label if entry.label else None,
                "comment: " + entry.comment if entry.comment else None,
            ]
            if x
        )
        for entry in properties
    ]
    embeddings = embedder(texts).astype(np.float32).tolist()
    processed_relations = [
        RDFPropertyProcessed(**rel.model_dump(), embedding=embedding)
        for rel, embedding in zip(properties, embeddings)
    ]
    return processed_relations


def transform_properties_preinsert(properties: list[RDFPropertyProcessed]):
    return [p.model_dump() for p in properties]


def make_property_search_schema(vector_dim: int):
    return (
        VectorField(
            "$.embedding",
            "FLAT",
            {"TYPE": "FLOAT32", "DIM": vector_dim, "DISTANCE_METRIC": "COSINE"},
            as_name="vector",
        ),
    )


def main(args: InsertThenIndexArgs):
    redis_client = Redis(host=args.redis_host, decode_responses=True)
    does_index_exist = get_index_existence(
        redis_client=redis_client, index_name=PROPERTIES_INDEX_NAME
    )

    if args.drop_index:
        if does_index_exist:
            print(
                "Index {index_name} exists but will be recreated.\n".format(
                    index_name=PROPERTIES_INDEX_NAME
                )
            )
            redis_client.ft(PROPERTIES_INDEX_NAME).dropindex(delete_documents=True)
    elif does_index_exist:
        print(
            "Index {index_name} exists; edge types have already been ingested.".format(
                index_name=PROPERTIES_INDEX_NAME
            )
        )
        return
    else:
        print(
            "Index {index_name} does not exist.\n".format(
                index_name=PROPERTIES_INDEX_NAME
            )
        )

    embedder = TritonEmbedder(url=args.text_embedding_url)
    vector_dim = len(embedder(["warmup"])[0])
    print("Vector dimension: ", vector_dim)
    schema = make_property_search_schema(vector_dim=vector_dim)

    ingester = DataIngester(
        dirname=PROPERTIES_DIRNAME,
        invalidate_cache=args.invalidate_cache,
        unprocessed_type=RDFProperty,
        processed_type=RDFPropertyProcessed,
        process_func=lambda properties: process_properties(
            embedder=embedder, properties=properties
        ),
        process_batchsize=PROPERTIES_PROCESS_BATCHSIZE,
        redis_client=redis_client,
        redis_key_prefix=PROPERTIES_KEY_PREFIX,
        redis_preinsert_transform=transform_properties_preinsert,
        redis_insert_batchsize=PROPERTIES_INSERT_BATCHSIZE,
        redis_index_name=PROPERTIES_INDEX_NAME,
        redis_ft_schema=schema,
    )
    ingester.ingest()
    print("Ingestion complete.")


if __name__ == "__main__":
    args = load_insert_then_index_args()
    main(args)
