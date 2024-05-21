from importlib.abc import Traversable
import importlib.resources
import json
import os
from typing import List

import numpy as np
from pydantic import TypeAdapter
from redis import Redis
from redis.commands.search.field import VectorField, TagField
from redis.commands.search.indexDefinition import IndexDefinition, IndexType

from services.embed import IEmbedder, TritonEmbedder
from services.redis import does_index_exist
from services.example_store.model import (
    Nlq2ActionExample,
    Nlq2ActionExampleProcessed,
    QADomain,
)


EXAMPLES_KEY_PREFIX = "nlq2actionExamples:"
EXAMPLES_INDEX_NAME = "idx:nlq2actionExamples_vss"


def discover_examples_groups():
    print("Discovering QA domains...")
    domain_dirs = [
        dir
        for dir in importlib.resources.files("data.examples").iterdir()
        if dir.is_dir() and dir.name != "__pycache__"
    ]
    print(
        "{num} QA domains discovered: {domains}.\n".format(
            num=len(domain_dirs), domains=", ".join(dir.name for dir in domain_dirs)
        )
    )

    print("Discovering groups of examples...")
    domain2folders = {
        domain_dir.name: [
            dir
            for dir in domain_dir.iterdir()
            if dir.is_dir() and dir.name != "__pycache__"
        ]
        for domain_dir in domain_dirs
    }
    print(
        "{num} groups of examples discovered: {groups}.\n".format(
            num=sum(len(folders) for folders in domain2folders.values()),
            groups=", ".join(
                folder.name for folders in domain2folders.values() for folder in folders
            ),
        )
    )

    return domain2folders


def load_processed_examples(
    adapter: TypeAdapter[List[Nlq2ActionExampleProcessed]], folder: Traversable
):
    text = folder.joinpath("processed.json").read_text()
    examples = adapter.validate_json(text)

    print("Found cache of processed examples for {name}.\n".format(name=folder.name))
    return examples


def save_processed_examples(
    adapter: TypeAdapter[List[Nlq2ActionExampleProcessed]],
    folder: Traversable,
    examples: List[Nlq2ActionExampleProcessed],
):
    path = folder.joinpath("processed.json")
    with open(str(path), "wb") as f:
        f.write(adapter.dump_json(examples))


def load_unprocessed_examples(qa_domain: QADomain, folder: Traversable):
    print("Loading unprocessed lexicon into memory...".format(clsname=folder.name))

    examples = [
        Nlq2ActionExample(qa_domain=qa_domain, nlq=row["input"], action=row["output"])
        for row in json.loads(folder.joinpath("examples.json").read_text())
    ]

    print("{num} examples loaded into memory.\n".format(num=len(examples)))
    return examples


def process_examples(embedder: IEmbedder, examples: List[Nlq2ActionExample]):
    print("Processing examples...")

    embeddings = (
        embedder([example.nlq for example in examples]).astype(np.float32).tolist()
    )
    processed_examples = [
        Nlq2ActionExampleProcessed(**example.model_dump(), nlq_embedding=embedding)
        for example, embedding in zip(examples, embeddings)
    ]
    print("Processing complete.\n")

    return processed_examples


def insert_examples(
    offset: int, redis_client: Redis, examples: List[Nlq2ActionExampleProcessed]
):
    print("Inserting examples into Redis...")

    pipeline = redis_client.pipeline()
    for i, example in enumerate(examples):
        redis_key = EXAMPLES_KEY_PREFIX + str(offset + i)
        doc = dict(
            qa_domain=example.qa_domain,
            nlq=example.nlq,
            action=json.dumps(example.action),
            nlq_embedding=example.nlq_embedding,
        )
        pipeline.json().set(redis_key, "$", doc)
    pipeline.execute()

    offset += len(examples)

    print("Insertion done.\n")
    return offset


def index_examples(redis_client: Redis, vector_dim: int):
    print("Indexing examples...")
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


def ingest_examples(redis_client: Redis):
    embedder = TritonEmbedder(url=os.environ["TEXT_EMBEDDING_URL"])

    domain2folders = discover_examples_groups()

    adapter = TypeAdapter(List[Nlq2ActionExampleProcessed])
    vector_dim = None
    offset = 0
    for qa_domain, folders in domain2folders.items():
        for folder in folders:
            try:
                examples = load_processed_examples(adapter=adapter, folder=folder)
            except:
                print(
                    "No cache of processed examples for {grp} found.".format(
                        grp=folder.name
                    )
                )
                examples = load_unprocessed_examples(qa_domain=qa_domain, folder=folder)
                examples = process_examples(embedder=embedder, examples=examples)
                save_processed_examples(
                    adapter=adapter, folder=folder, examples=examples
                )

            if vector_dim is None:
                vector_dim = len(examples[0].nlq_embedding)

            offset = insert_examples(
                offset=offset, redis_client=redis_client, examples=examples
            )

    assert vector_dim is not None
    index_examples(redis_client=redis_client, vector_dim=vector_dim)


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
