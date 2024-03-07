from functools import cache
from typing import Annotated, List

from fastapi import Depends
import numpy as np
from redis.commands.search.field import (
    TextField,
    VectorField,
)
from redis.commands.search.indexDefinition import IndexDefinition, IndexType
from redis.commands.search.query import Query

from services.kg_client import KgClient
from services.embed import IEmbedder, get_embedder
from services.redis_client import get_redis_client
from .kg_client import get_ontospecies_kg_client


class OntoSpeciesAligner:
    CHEMICAL_CLASS_KEY_PREFIX = "ontospecies:chemical_classes:"
    CHEMICAL_CLASS_INDEX_NAME = "idx:chemical_classes_vss"
    KNN_QUERY = (
        Query("(*)=>[KNN 3 @vector $query_vector AS vector_score]")
        .sort_by("vector_score")
        .return_fields("vector_score", "label")
        .dialect(2)
    )

    def __init__(
        self,
        kg_client: Annotated[KgClient, Depends(get_ontospecies_kg_client)],
        embedder: Annotated[IEmbedder, Depends(get_embedder)],
        cosine_similarity_threshold: float = 0.5,
    ):
        self.redis_client = get_redis_client()
        self.kg_client = kg_client
        self.embedder = embedder
        self.cosine_similarity_threshold = cosine_similarity_threshold

    def _get_chemical_classes(self):
        query = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>

SELECT DISTINCT ?Label WHERE {
    ?s a os:ChemicalClass ; rdfs:label ?Label .
}"""
        return [
            x["Label"]["value"]
            for x in self.kg_client.query(query)["results"]["bindings"]
        ]

    def embed_chemical_classes(self):
        chemical_classes = self._get_chemical_classes()
        embeddings = self.embedder(chemical_classes).astype(np.float32).tolist()
        vector_dim = len(embeddings[0])

        pipeline = self.redis_client.pipeline()
        for i, (chemclass, embedding) in enumerate(zip(chemical_classes, embeddings)):
            redis_key = self.CHEMICAL_CLASS_KEY_PREFIX + str(i)
            pipeline.json().set(
                redis_key, "$", dict(label=chemclass, label_embedding=embedding)
            )
        pipeline.execute()

        schema = (
            TextField("$.label", no_stem=True, as_name="label"),
            VectorField(
                "$.label_embedding",
                "FLAT",
                {"TYPE": "FLOAT32", "DIM": vector_dim, "DISTANCE_METRIC": "COSINE"},
                as_name="vector",
            ),
        )
        definition = IndexDefinition(
            prefix=[self.CHEMICAL_CLASS_KEY_PREFIX], index_type=IndexType.JSON
        )
        self.redis_client.ft(self.CHEMICAL_CLASS_INDEX_NAME).create_index(
            fields=schema, definition=definition
        )

    @cache
    def does_index_exist(self, index_name: str):
        if self.redis_client.ft(index_name).info():
            return True
        return False

    def align_chemical_classes(self, chemical_classes: List[str]):
        if not self.does_index_exist(self.CHEMICAL_CLASS_INDEX_NAME):
            self.embed_chemical_classes()

        encoded_queries = self.embedder(chemical_classes).astype(np.float32)

        aligned_lst: List[List[str]] = []
        for encoded_query in encoded_queries:
            docs = (
                self.redis_client.ft(self.CHEMICAL_CLASS_INDEX_NAME)
                .search(
                    self.KNN_QUERY, {"query_vector": encoded_query.tobytes()}
                )
                .docs
            )
            filtered_docs = [
                doc
                for doc in docs
                if float(doc.vector_score) < self.cosine_similarity_threshold
            ]
            if not filtered_docs:
                aligned = [docs[0].label]
            else:
                aligned = [doc.label for doc in filtered_docs]
            aligned_lst.append(aligned)

        return aligned_lst
