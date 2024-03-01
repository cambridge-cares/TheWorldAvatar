from typing import Dict, List

import numpy as np

from .openai_client import openai_client


def cos_sim(a: np.ndarray, b: np.ndarray):
    return a @ b.T / (np.linalg.norm(a) * np.linalg.norm(b))


class NNRetriever:
    def __init__(self, model: str = "text-embedding-3-small"):
        self.model = model
        self.doc_cache: Dict[str, np.ndarray] = dict()

    def _compute_embeddings(self, documents: List[str]):
        return [
            x.embedding
            for x in openai_client.embeddings.create(
                input=documents, model=self.model
            ).data
        ]

    def _retrieve_embeddings(self, documents: List[str]):
        new_docs = [x for x in documents if x not in self.doc_cache]
        new_embeds = self._compute_embeddings(new_docs)
        for doc, embed in zip(new_docs, new_embeds):
            self.doc_cache[doc] = embed

        return np.array([self.doc_cache[doc] for doc in documents])

    def retrieve(self, documents: List[str], queries: List[str]):
        doc_embeds = self._retrieve_embeddings(documents)
        query_embeds = np.array(self._compute_embeddings(queries))

        cosine_scores = cos_sim(doc_embeds, query_embeds)
        closest_idxes: List[int] = cosine_scores.argmax(axis=0)
        return [documents[idx] for idx in closest_idxes]
