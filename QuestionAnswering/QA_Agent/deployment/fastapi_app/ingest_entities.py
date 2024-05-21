import importlib.resources
from importlib.abc import Traversable
import json
import math
import os
from typing import List

import numpy as np
from pydantic import TypeAdapter
from redis import Redis
from redis.commands.search.field import TextField, VectorField, TagField
from redis.commands.search.indexDefinition import IndexDefinition, IndexType
import regex
from tqdm import tqdm

from utils.itertools_recipes import batched
from services.embed import IEmbedder, TritonEmbedder
from services.redis import does_index_exist
from services.entity_store.model import LexiconEntry, LexiconEntryProcessed


ENTITIES_KEY_PREFIX = "entities:"
ENTITIES_INDEX_NAME = "idx:entities"
ENTITIES_CHUNK_SIZE = 1024


def discover_lexicon_groups():
    print("Discovering lexicon groups...")
    folders = [
        dir
        for dir in importlib.resources.files("data.lexicon").iterdir()
        if dir.is_dir() and dir.name != "__pycache__"
    ]
    print("{num} lexicon groups discovered.\n".format(num=len(folders)))

    return folders


def load_processed_lexicon(
    adapter: TypeAdapter[List[LexiconEntryProcessed]], folder: Traversable
):
    text = folder.joinpath("processed.json").read_text()
    entries = adapter.validate_json(text)

    print("Found cache of processed lexicon for {name}.\n".format(name=folder.name))
    return entries


def save_processed_lexicon(
    adapter: TypeAdapter[List[LexiconEntryProcessed]],
    folder: Traversable,
    entries: List[LexiconEntryProcessed],
):
    path = folder.joinpath("processed.json")

    with open(str(path), "wb") as f:
        f.write(adapter.dump_json(entries))


def load_unprocessed_lexicon(folder: Traversable):
    print("Loading unprocessed lexicon into memory...".format(clsname=folder.name))

    entries = json.loads(folder.joinpath("lexicon.json").read_text())
    for entry in entries:
        entry["clsname"] = folder.name

    entries = [LexiconEntry.model_validate(entry) for entry in entries]

    print("{num} lexicon entries loaded into memory.\n".format(num=len(entries)))
    return entries


def process_lexicon(embedder: IEmbedder, entries: List[LexiconEntry]):
    print(
        "Processing {clsname} lexicon with chunk size {chunk_size}...".format(
            clsname=entries[0].clsname, chunk_size=ENTITIES_CHUNK_SIZE
        )
    )

    processed_entries: List[LexiconEntryProcessed] = []
    for chunk in tqdm(
        batched(entries, ENTITIES_CHUNK_SIZE),
        total=math.ceil(len(entries) / ENTITIES_CHUNK_SIZE),
    ):
        chunk = list(chunk)
        embeddings = (
            embedder([", ".join(entry.surface_forms) for entry in chunk])
            .astype(np.float32)
            .tolist()
        )
        processed_entries.extend(
            LexiconEntryProcessed(
                **entry.model_dump(), surface_forms_embedding=embedding
            )
            for entry, embedding in zip(chunk, embeddings)
        )

    print("Processing complete.\n")
    return processed_entries


def insert_entities(
    offset: int, redis_client: Redis, entries: List[LexiconEntryProcessed]
):
    print(
        "Inserting {clsname} into Redis with chunk size {chunk_size}...".format(
            clsname=entries[0].clsname, chunk_size=ENTITIES_CHUNK_SIZE
        )
    )

    for chunk in tqdm(
        batched(entries, ENTITIES_CHUNK_SIZE),
        total=math.ceil(len(entries) / ENTITIES_CHUNK_SIZE),
    ):
        pipeline = redis_client.pipeline()
        for i, entry in enumerate(chunk):
            redis_key = ENTITIES_KEY_PREFIX + str(offset + i)
            doc = dict(
                iri=entry.iri,
                clsname=entry.clsname,
                label=regex.escape(
                    entry.label, special_only=False, literal_spaces=True
                ),
                surface_forms=[
                    regex.escape(sf, special_only=False, literal_spaces=True)
                    for sf in entry.surface_forms
                ],
                surface_forms_embedding=entry.surface_forms_embedding,
            )
            pipeline.json().set(redis_key, "$", doc)
        pipeline.execute()

        offset += len(chunk)

    print("Insertion done.\n")
    return offset


def index_entities(redis_client: Redis, vector_dim: int):
    print("Indexing entities...")

    schema = (
        TagField("$.iri", as_name="iri"),
        TagField("$.clsname", as_name="clsname"),
        TextField("$.label", as_name="label"),
        TextField("$.surface_forms.*", as_name="surface_forms"),
        VectorField(
            "$.surface_forms_embedding",
            "FLAT",
            {"TYPE": "FLOAT32", "DIM": vector_dim, "DISTANCE_METRIC": "IP"},
            as_name="vector",
        ),
    )
    definition = IndexDefinition(
        prefix=[ENTITIES_KEY_PREFIX], index_type=IndexType.JSON
    )
    redis_client.ft(ENTITIES_INDEX_NAME).create_index(
        fields=schema, definition=definition
    )

    print("Index {index} created\n".format(index=ENTITIES_INDEX_NAME))


def ingest_entities(redis_client: Redis):
    embedder = TritonEmbedder(url=os.environ["TEXT_EMBEDDING_URL"])

    folders = discover_lexicon_groups()

    adapter = TypeAdapter(List[LexiconEntryProcessed])
    vector_dim = None
    offset = 0
    for folder in folders:
        try:
            entries = load_processed_lexicon(adapter=adapter, folder=folder)
        except:
            print(
                "No cache of processed lexicon for {clsname} found.".format(
                    clsname=folder.name
                )
            )
            entries = load_unprocessed_lexicon(folder)
            entries = process_lexicon(embedder=embedder, entries=entries)
            save_processed_lexicon(adapter=adapter, folder=folder, entries=entries)

        if vector_dim is None:
            vector_dim = len(entries[0].surface_forms_embedding)

        offset = insert_entities(
            offset=offset, redis_client=redis_client, entries=entries
        )

    assert vector_dim is not None

    index_entities(redis_client=redis_client, vector_dim=vector_dim)


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
        ingest_entities(redis_client)
        print("Ingestion complete.")
