from importlib.abc import Traversable
import importlib.resources
import json
import math
import os
from typing import List

import numpy as np
from pydantic import TypeAdapter
from redis import Redis
from redis.commands.search.field import VectorField, TagField
from redis.commands.search.indexDefinition import IndexDefinition, IndexType
from tqdm import tqdm

from utils.itertools_recipes import batched
from services.embed import TritonEmbedder
from services.redis import does_index_exist
from services.example_store.model import (
    Nlq2ActionExample,
    Nlq2ActionExampleProcessed,
    QADomain,
)


EXAMPLES_KEY_PREFIX = "nlq2actionExamples:"
EXAMPLES_INDEX_NAME = "idx:nlq2actionExamples_vss"
EXAMPLES_CHUNK_SIZE = 256


def get_processed_examples_path():
    return importlib.resources.files("data").joinpath("examples_processed.json")


def load_processed_examples():
    adapter = TypeAdapter(List[Nlq2ActionExampleProcessed])
    text = get_processed_examples_path().read_text()
    return adapter.validate_json(text)


def save_processed_examples(examples: List[Nlq2ActionExampleProcessed]):
    adapter = TypeAdapter(List[Nlq2ActionExampleProcessed])
    path = get_processed_examples_path()
    with open(str(path), "wb") as f:
        f.write(adapter.dump_json(examples))


def load_example_file(qa_domain: QADomain, file: Traversable):
    return [
        Nlq2ActionExample(
            qa_domain=qa_domain,
            nlq=example["input"],
            action=example["output"],
        )
        for example in json.loads(file.read_text())
    ]


def load_unprocessed_examples():
    print("Discovering QA domains...")
    dirs = [
        dir
        for dir in importlib.resources.files("data.examples").iterdir()
        if dir.is_dir()
    ]
    print(
        "{num} QA domains discovered: {domains}.\n".format(
            num=len(dirs), domains=", ".join(dir.name for dir in dirs)
        )
    )

    print("Loading examples into memory...")
    domain_file_pairs = [
        (dir.name, file)
        for dir in dirs
        for file in dir.iterdir()
        if file.is_file() and file.name.lower().endswith("_examples.json")
    ]
    examples = [
        example
        for qa_domain, file in domain_file_pairs
        for example in load_example_file(qa_domain=qa_domain, file=file)
    ]
    print("All {num} examples loaded.\n".format(num=len(examples)))

    return examples


def process_examples(examples: List[Nlq2ActionExample]):
    embedder = TritonEmbedder(url=os.environ["TEXT_EMBEDDING_URL"])

    print(
        "Genearting embeddings of NLQs with chunk size {chunksize}".format(
            chunksize=EXAMPLES_CHUNK_SIZE
        )
    )
    processed_examples: List[Nlq2ActionExampleProcessed] = []
    for chunk in tqdm(
        batched(examples, EXAMPLES_CHUNK_SIZE),
        total=math.ceil(len(examples / EXAMPLES_CHUNK_SIZE)),
    ):
        chunk = list(chunk)
        embeddings = (
            embedder([example.nlq for example in chunk]).astype(np.float32).tolist()
        )
        processed_examples.extend(
            Nlq2ActionExampleProcessed(**example.model_dump(), nlq_embedding=embedding)
            for example, embedding in zip(chunk, embeddings)
        )
    print("Embedding generation complete.\n")

    return processed_examples


def insert_then_index_examples(
    redis_client: Redis, examples: List[Nlq2ActionExampleProcessed]
):
    offset = 0

    print("Inserting examples into Redis...")

    for chunk in batched(examples, 10):
        pipeline = redis_client.pipeline()
        for i, example in enumerate(chunk):
            redis_key = EXAMPLES_KEY_PREFIX + str(offset + i)
            doc = dict(
                qa_domain=example.qa_domain,
                nlq=example.nlq,
                action=json.dumps(example.action),
                nlq_embedding=example.nlq_embedding,
            )
            pipeline.json().set(redis_key, "$", doc)
        pipeline.execute()

        offset += len(chunk)

    print("Insertion done.\n")

    print("Indexing examples...")
    vector_dim = len(examples[0].nlq_embedding)
    schema = (
        TagField("$.qa_domain", as_name="qa_domain"),
        VectorField(
            "$.nlq_embedding",
            "FLAT",
            {"TYPE": "FLOAT32", "DIM": vector_dim, "DISTANCE_METRIC": "IP"},
            as_name="vector",
        ),
    )
    definition = IndexDefinition(
        prefix=[EXAMPLES_KEY_PREFIX], index_type=IndexType.JSON
    )
    redis_client.ft(EXAMPLES_INDEX_NAME).create_index(
        fields=schema, definition=definition
    )

    print("Index {index} created.\n".format(index=EXAMPLES_INDEX_NAME))


def load_examples():
    try:
        examples = load_processed_examples()
        print("A cache of processed examples found.\n")
    except:
        print("No cache of processed examples found. Initiating lexicon discovery and processing tasks...\n")
        examples = load_unprocessed_examples()
        examples = process_examples(examples)
        save_processed_examples(examples)

    return examples


def ingest_examples(redis_client: Redis):
    examples = load_examples()
    insert_then_index_examples(redis_client=redis_client, examples=examples)


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
            "Index {index_name} does not exist. Initiating ingestion...\n".format(
                index_name=EXAMPLES_INDEX_NAME
            )
        )
        ingest_examples(redis_client)
        print("Ingestion complete.")
