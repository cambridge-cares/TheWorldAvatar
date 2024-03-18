from typing import Annotated, List, Tuple

from fastapi import Depends

from services.kg_client import KgClient
from services.retrieve_docs import DocsRetriever, get_docs_retriever
from ..kg_client import get_ontospecies_kg_client


class OntoSpeciesLiteralAligner:
    def __init__(
        self,
        kg_client: KgClient,
        docs_retriever: DocsRetriever,
        cosine_similarity_threshold: float = 0.5,
    ):
        self.kg_client = kg_client
        self.docs_retriever = docs_retriever
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

    def _get_uses(self):
        query = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>

SELECT DISTINCT ?Label WHERE {
    ?s a os:Use ; rdfs:label ?Label .
}"""
        return [
            x["Label"]["value"]
            for x in self.kg_client.query(query)["results"]["bindings"]
        ]

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
        docs_scores_lst = self.docs_retriever.retrieve(
            queries=chemical_classes,
            key="ontospecies:chemical_classes",
            docs_getter=self._get_chemical_classes,
        )
        return [self._filter(docs_scores) for docs_scores in docs_scores_lst]

    def align_uses(self, uses: List[str]):
        docs_scores_lst = self.docs_retriever.retrieve(
            queries=uses,
            key="ontospecies:uses",
            docs_getter=self._get_uses,
        )
        return [self._filter(docs_scores) for docs_scores in docs_scores_lst]


def get_ontospecies_literal_aligner(
    kg_client: Annotated[KgClient, Depends(get_ontospecies_kg_client)],
    docs_retriever: Annotated[DocsRetriever, Depends(get_docs_retriever)],
):
    return OntoSpeciesLiteralAligner(kg_client=kg_client, docs_retriever=docs_retriever)
