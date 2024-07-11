import numpy as np
from redis import Redis
from redis.commands.search.field import TextField, VectorField, TagField
import regex

from config import get_app_settings
from services.ingest import (
    DataIngester,
    InsertThenIndexArgs,
    load_insert_then_index_args,
)
from services.embed import IEmbedder, TritonEmbedder
from services.redis import get_index_existence
from model.lexicon import (
    ENTITIES_INDEX_NAME,
    ENTITIES_KEY_PREFIX,
    LexiconEntry,
    LexiconEntryProcessed,
)


ENTITIES_DIRNAME = "lexicon"
ENTITIES_PROCESS_BATCHSIZE = 512
ENTITIES_INSERT_BATCHSIZE = 1024


def process_lexicon(
    embedder: IEmbedder, make_embedding: bool, data: list[LexiconEntry]
):
    embeddings = (
        (
            embedder([", ".join(entry.surface_forms) for entry in data])
            .astype(np.float32)
            .tolist()
        )
        if make_embedding
        else [None for _ in data]
    )
    return [
        LexiconEntryProcessed(**entry.model_dump(), surface_forms_embedding=embedding)
        for entry, embedding in zip(data, embeddings)
    ]


def lexicon_preinsert_transform(data: list[LexiconEntryProcessed]):
    docs = [
        {
            "iri": entry.iri,
            "cls": entry.cls,
            "label": regex.escape(entry.label, special_only=False, literal_spaces=True),
            "surface_forms": [
                regex.escape(sf, special_only=False, literal_spaces=True)
                for sf in entry.surface_forms
            ],
            "surface_forms_embedding": entry.surface_forms_embedding,
        }
        for entry in data
    ]
    return [{k: v for k, v in doc.items() if v} for doc in docs]


def make_entity_search_schema(vector_dim: int):
    return (
        TagField("$.iri", as_name="iri"),
        TagField("$.cls", as_name="cls"),
        TextField("$.label", as_name="label"),
        TextField("$.surface_forms.*", as_name="surface_forms"),
        VectorField(
            "$.surface_forms_embedding",
            "FLAT",
            {
                "TYPE": "FLOAT32",
                "DIM": vector_dim,
                "DISTANCE_METRIC": "COSINE",
            },
            as_name="vector",
        ),
    )


def main(args: InsertThenIndexArgs):
    redis_client = Redis(host=args.redis_host, decode_responses=True)
    does_index_exist = get_index_existence(
        redis_client=redis_client, index_name=ENTITIES_INDEX_NAME
    )

    if args.drop_index:
        if does_index_exist:
            print(
                "Index {index_name} exists but will be recreated.\n".format(
                    index_name=ENTITIES_INDEX_NAME
                )
            )
            redis_client.ft(ENTITIES_INDEX_NAME).dropindex(delete_documents=True)
    elif does_index_exist:
        print(
            "Index {index_name} exists; entity lexicons have already been ingested.".format(
                index_name=ENTITIES_INDEX_NAME
            )
        )
        return
    else:
        print(
            "Index {index_name} does not exist.\n".format(
                index_name=ENTITIES_INDEX_NAME
            )
        )

    embedder = TritonEmbedder(url=args.text_embedding_url)
    el_configs = get_app_settings().entity_linking.entries
    cls2config = {config.cls: config for config in el_configs}

    def process_func(data: list[LexiconEntry]):
        el_config = cls2config.get(data[0].cls)
        make_embedding = (
            True if el_config and el_config.strategy == "semantic" else False
        )
        return process_lexicon(
            embedder=embedder,
            make_embedding=make_embedding,
            data=data,
        )

    vector_dim = len(embedder(["warmup"])[0])
    print("Vector dimension: ", vector_dim)
    schema = make_entity_search_schema(vector_dim=vector_dim)

    ingester = DataIngester(
        dirname=ENTITIES_DIRNAME,
        invalidate_cache=args.invalidate_cache,
        unprocessed_type=LexiconEntry,
        processed_type=LexiconEntryProcessed,
        process_func=process_func,
        process_batchsize=ENTITIES_PROCESS_BATCHSIZE,
        redis_client=redis_client,
        redis_key_prefix=ENTITIES_KEY_PREFIX,
        redis_preinsert_transform=lexicon_preinsert_transform,
        redis_insert_batchsize=ENTITIES_INSERT_BATCHSIZE,
        redis_index_name=ENTITIES_INDEX_NAME,
        redis_ft_schema=schema,
    )
    ingester.ingest()
    print("Ingestion complete.")


if __name__ == "__main__":
    args = load_insert_then_index_args()
    main(args)
