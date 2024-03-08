from functools import cache
from typing import Annotated, List, Tuple, Type

from fastapi import Depends

from services.retrieve_docs import DocsRetriever, get_docs_retriever
from services.kg_client import KgClient
from .constants import (
    SpeciesAttrKey,
    SpeciesChemicalClassAttrKey,
    SpeciesIdentifierAttrKey,
    SpeciesPropertyAttrKey,
    SpeciesUseAttrKey,
)
from .kg_client import get_ontospecies_kg_client


class OntoSpeciesAligner:
    _SPECIES_ATTR_CLSES: List[Type[SpeciesAttrKey]] = [
        SpeciesChemicalClassAttrKey,
        SpeciesUseAttrKey,
        SpeciesIdentifierAttrKey,
        SpeciesPropertyAttrKey,
    ]
    _SPECIES_ATTR_KEYS = [x.value for cls in _SPECIES_ATTR_CLSES for x in cls]

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

    def align_property_keys(self, properties: List[str]):
        queries = [
            "".join([w.capitalize() for w in prop.split()]) for prop in properties
        ]
        docs_scores_lst = self.docs_retriever.retrieve(
            queries=queries,
            key="ontospecies:property_keys",
            docs_getter=lambda: [x.value for x in SpeciesPropertyAttrKey],
            k=1,
        )
        return [
            SpeciesPropertyAttrKey(docs_scores[0][0]) for docs_scores in docs_scores_lst
        ]

    def align_attribute_keys(self, attributes: List[str]):
        queries = [
            "".join([w.capitalize() for w in attr.split()]) for attr in attributes
        ]
        docs_scores_lst = self.docs_retriever.retrieve(
            queries=queries,
            key="ontospecies:property_keys",
            docs_getter=lambda: self._SPECIES_ATTR_KEYS,
            k=1,
        )
        return [
            SpeciesPropertyAttrKey(docs_scores[0][0]) for docs_scores in docs_scores_lst
        ]


@cache
def get_ontospecies_aligner(
    kg_client: Annotated[KgClient, Depends(get_ontospecies_kg_client)],
    docs_retriever: Annotated[DocsRetriever, Depends(get_docs_retriever)],
):
    return OntoSpeciesAligner(kg_client, docs_retriever)
