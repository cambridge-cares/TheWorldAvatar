import os
from typing import List

import numpy as np
from redis import Redis
from redis.commands.search.field import TextField, VectorField, TagField
import regex

from services.ingest import DataIngester
from services.entity_store import get_clsname2config
from services.embed import IEmbedder, TritonEmbedder
from services.redis import does_index_exist
from services.entity_store.model import (
    ENTITIES_INDEX_NAME,
    ENTITIES_KEY_PREFIX,
    LexiconEntry,
    LexiconEntryProcessed,
)


ENTITIES_DIRNAME = "lexicon"
ENTITIES_PROCESS_BATCHSIZE = 512
ENTITIES_INSERT_BATCHSIZE = 1024


def process_lexicon(
    embedder: IEmbedder, make_embedding: bool, data: List[LexiconEntry]
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


def lexicon_preinsert_transform(data: List[LexiconEntryProcessed]):
    docs = [
        {
            "iri": entry.iri,
            "clsname": entry.clsname,
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
        TagField("$.clsname", as_name="clsname"),
        TextField("$.label", as_name="label"),
        TextField("$.surface_forms.*", as_name="surface_forms"),
        VectorField(
            "$.surface_forms_embedding",
            "FLAT",
            {
                "TYPE": "FLOAT32",
                "DIM": vector_dim,
                "DISTANCE_METRIC": "IP",
            },
            as_name="vector",
        ),
    )


if __name__ == "__main__":
    redis_client = Redis(
        host=os.getenv("REDIS_HOST", "localhost"), decode_responses=True
    )
    if does_index_exist(redis_client=redis_client, index_name=ENTITIES_INDEX_NAME):
        print(
            "Index {index_name} exists; entities have already been ingested".format(
                index_name=ENTITIES_INDEX_NAME
            )
        )
    else:
        print(
            "Index {index_name} does not exist. Initiating ingestion...".format(
                index_name=ENTITIES_INDEX_NAME
            )
        )

        embedder = TritonEmbedder(url=os.environ["TEXT_EMBEDDING_URL"])

        clsname2config = get_clsname2config()

        def process_func(data: List[LexiconEntry]):
            el_config = clsname2config.get(data[0].clsname)
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
