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
from services.embed import TritonEmbedder
from services.redis import does_index_exist
from services.entity_store.model import LexiconEntry, LexiconEntryProcessed


ENTITIES_KEY_PREFIX = "entities:"
ENTITIES_INDEX_NAME = "idx:entities"
ENTITIES_CHUNK_SIZE = 1024


def get_processed_lexicon_path():
    return importlib.resources.files("data").joinpath("lexicon_processed.json")


def load_processed_lexicon():
    adapter = TypeAdapter(List[LexiconEntryProcessed])
    text = get_processed_lexicon_path().read_text()
    return adapter.validate_json(text)


def save_processed_lexicon(entries: List[LexiconEntryProcessed]):
    adapter = TypeAdapter(List[LexiconEntryProcessed])
    path = get_processed_lexicon_path()
    with open(str(path), "wb") as f:
        f.write(adapter.dump_json(entries))


def load_lexicon_file(file: Traversable):
    entries = json.loads(file.read_text())
    clsname = file.name[: -len("_lexicon.json")]

    for entry in entries:
        entry["clsname"] = clsname

    return [LexiconEntry.model_validate(entry) for entry in entries]


def load_unprocessed_lexicon():
    print("Discovering lexicon files...")
    files = [
        file
        for file in importlib.resources.files("data.lexicon").iterdir()
        if file.is_file() and file.name.endswith("_lexicon.json")
    ]
    print("{num} lexicon files discovered.".format(num=len(files)))
    print()

    print("Loading lexicon files...")
    entries = [entry for file in files for entry in load_lexicon_file(file)]
    print("All {num} lexicon files loaded to memory.".format(num=len(entries)))
    print()

    return entries


def process_lexicon(entries: List[LexiconEntry]):
    embedder = TritonEmbedder(url=os.environ["TEXT_EMBEDDING_URL"])

    print(
        "Generating embeddings of entities' surface forms with chunk size = {chunksize}...".format(
            chunksize=ENTITIES_CHUNK_SIZE
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
    print("Embedding generation complete.")
    print()

    return processed_entries


def load_lexicon():
    try:
        entries = load_processed_lexicon()
        print("A cache of processed lexicon found.\n")
    except:
        print("No cache of processed lexicon found.\n")
        entries = load_unprocessed_lexicon()
        entries = process_lexicon(entries)
        save_processed_lexicon(entries)
    return entries


def insert_then_index_entities(
    redis_client: Redis, entries: List[LexiconEntryProcessed]
):
    print("Inserting entities into Redis...")
    print("Chunk size: " + str(ENTITIES_CHUNK_SIZE))

    offset = 0
    for chunk in tqdm(batched(entries, ENTITIES_CHUNK_SIZE)):
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

    print("Insertion done")
    print()

    print("Indexing entities...")

    vector_dim = len(entries[0].surface_forms_embedding)
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

    print("Index {index} created".format(index=ENTITIES_INDEX_NAME))
    print()


def ingest_entities(redis_client: Redis):
    entries = load_lexicon()
    insert_then_index_entities(redis_client=redis_client, entries=entries)


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
