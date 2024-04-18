from typing import Annotated, List, Tuple

from fastapi import Depends
from redis import Redis

from services.core.embed import IEmbedder, get_embedder
from services.core.redis import get_redis_client
from services.core.kg import KgClient
from services.core.retrieve_docs import DocsRetriever
from ..kg import get_ontospecies_kg_client


class OntoSpeciesLiteralAligner:
    @classmethod
    def _get_chemical_classes(cls, kg_client: KgClient):
        query = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>

SELECT DISTINCT ?Label WHERE {
    ?s a os:ChemicalClass ; rdfs:label ?Label .
}"""
        for binding in kg_client.query(query)["results"]["bindings"]:
            yield binding["Label"]["value"]

    @classmethod
    def _get_uses(cls, kg_client: KgClient):
        query = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>

SELECT DISTINCT ?Label WHERE {
    ?s a os:Use ; rdfs:label ?Label .
}"""
        for binding in kg_client.query(query)["results"]["bindings"]:
            yield binding["Label"]["value"]

    def __init__(
        self,
        embedder: IEmbedder,
        redis_client: Redis,
        kg_client: KgClient,
        cosine_similarity_threshold: float = 0.5,
    ):
        self.kg_client = kg_client
        self.chemical_classes_retriever = DocsRetriever(
            embedder=embedder,
            redis_client=redis_client,
            key="ontospecies:chemical_classes",
            docs=self._get_chemical_classes(kg_client),
        )
        self.uses_retriever = DocsRetriever(
            embedder=embedder,
            redis_client=redis_client,
            key="ontospecies:uses",
            docs=self._get_uses(kg_client),
        )
        self.cosine_similarity_threshold = cosine_similarity_threshold

    def _filter(self, docs_scores: List[Tuple[str, float]]):
        """Filters for docs below cosine similarity threshold.
        If all docs are below the threshold, return the first doc."""
        docs = [
            doc
            for doc, score in docs_scores
            if score < self.cosine_similarity_threshold
        ]
        if not docs:
            docs = [docs_scores[0][0]]
        return docs

    def align_chemical_classes(self, chemical_classes: List[str]):
        docs_scores_lst = self.chemical_classes_retriever.retrieve(
            queries=chemical_classes
        )
        return [self._filter(docs_scores) for docs_scores in docs_scores_lst]

    def align_uses(self, uses: List[str]):
        docs_scores_lst = self.uses_retriever.retrieve(queries=uses)
        return [self._filter(docs_scores) for docs_scores in docs_scores_lst]


def get_ontospecies_literal_aligner(
    embedder: Annotated[IEmbedder, Depends(get_embedder)],
    redis_client: Annotated[Redis, Depends(get_redis_client)],
    kg_client: Annotated[KgClient, Depends(get_ontospecies_kg_client)],
):
    return OntoSpeciesLiteralAligner(
        embedder=embedder,
        redis_client=redis_client,
        kg_client=kg_client,
    )
