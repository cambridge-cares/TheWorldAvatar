import os
from typing import Dict, List
import importlib.resources

import numpy as np
from pydantic import TypeAdapter
from redis import Redis
from redis.commands.search.field import VectorField

from services.embed import IEmbedder, TritonEmbedder
from services.ingest import DataIngester
from services.redis import does_index_exist
from services.schema_store.model import (
    RELATIONS_INDEX_NAME,
    RELATIONS_KEY_PREFIX,
    RDFItemAnnotated,
    RDFItemAnnotation,
    RDFRelation,
    RDFRelationProcessed,
)
from utils.rdf import extract_name


RELATIONS_DIRNAME = "schema/relations"
RELATIONS_PROCESS_BATCHSIZE = 512
RELATIONS_INSERT_BATCHSIZE = 512


def format_rdf_item(iri: str, iri2annotation: Dict[str, RDFItemAnnotation]):
    name = extract_name(iri)
    annotation = iri2annotation.get(iri, RDFItemAnnotation())
    if annotation.label:
        return annotation.label
    elif annotation.comment:
        return "{name} ({comment})".format(name=name, comment=annotation.comment)
    else:
        return name


def process_relations(
    embedder: IEmbedder,
    iri2annotation: Dict[str, RDFItemAnnotation],
    relations: List[RDFRelation],
):
    triples = [
        "{s} {p} {o}".format(
            s=format_rdf_item(iri=rel.s, iri2annotation=iri2annotation),
            p=format_rdf_item(iri=rel.p, iri2annotation=iri2annotation),
            o=format_rdf_item(iri=rel.o, iri2annotation=iri2annotation),
        )
        for rel in relations
    ]
    embeddings = embedder(triples).astype(np.float32).tolist()
    processed_relations = [
        RDFRelationProcessed(
            **rel.model_dump(), triple_repr=triple, triple_repr_embedding=embedding
        )
        for rel, triple, embedding in zip(relations, triples, embeddings)
    ]
    return processed_relations


def transform_relations_preinsert(relations: List[RDFRelationProcessed]):
    return [rel.model_dump() for rel in relations]


def make_relation_search_schema(vector_dim: int):
    return (
        VectorField(
            "$.triple_repr_embedding",
            "FLAT",
            {"TYPE": "FLOAT32", "DIM": vector_dim, "DISTANCE_METRIC": "IP"},
            as_name="vector",
        ),
    )


if __name__ == "__main__":
    redis_client = Redis(
        host=os.getenv("REDIS_HOST", "localhost"), decode_responses=True
    )

    if does_index_exist(redis_client=redis_client, index_name=RELATIONS_INDEX_NAME):
        print(
            "Index {index_name} exists; schema descriptions have already been ingested.".format(
                index_name=RELATIONS_INDEX_NAME
            )
        )
    else:
        print(
            "Index {index_name} does not exist.\n".format(
                index_name=RELATIONS_INDEX_NAME
            )
        )

        texts = [
            file.read_text()
            for file in importlib.resources.files("data")
            .joinpath("schema/annotations")
            .iterdir()
            if file.is_file() and file.name.endswith(".json")
        ]
        adapter = TypeAdapter(List[RDFItemAnnotated])
        items = [item for text in texts for item in adapter.validate_json(text)]
        iri2annotation = {item.iri: item.annotation for item in items}

        embedder = TritonEmbedder(url=os.environ["TEXT_EMBEDDING_URL"])
        vector_dim = len(embedder(["warmup"])[0])
        print("Vector dimension: ", vector_dim)
        schema = make_relation_search_schema(vector_dim=vector_dim)

        ingester = DataIngester(
            dirname=RELATIONS_DIRNAME,
            unprocessed_type=RDFRelation,
            processed_type=RDFRelationProcessed,
            process_func=lambda relations: process_relations(
                embedder=embedder, iri2annotation=iri2annotation, relations=relations
            ),
            process_batchsize=RELATIONS_PROCESS_BATCHSIZE,
            redis_client=redis_client,
            redis_key_prefix=RELATIONS_KEY_PREFIX,
            redis_preinsert_transform=transform_relations_preinsert,
            redis_insert_batchsize=RELATIONS_INSERT_BATCHSIZE,
            redis_index_name=RELATIONS_INDEX_NAME,
            redis_ft_schema=schema,
        )
        ingester.ingest()
        print("Ingestion complete.")
