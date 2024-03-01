from typing import List

import numpy as np

from services.embed import IEmbedder
from services.cache import ICache


def cos_sim(a: np.ndarray, b: np.ndarray):
    a = a / np.linalg.norm(a, axis=1)[:, None]
    b = b / np.linalg.norm(b, axis=1)[:, None]
    return a @ b.T


class NNRetriever:
    def __init__(
        self,
        embedder: IEmbedder,
        cache: ICache[str, List[float]],
    ):
        self.embeder = embedder
        self.cache = cache

    def _retrieve_embeddings(self, documents: List[str]):
        new_docs = [doc for doc in documents if not self.cache.exists(doc)]
        new_embeds = self.embeder(new_docs)
        for doc, embed in zip(new_docs, new_embeds):
            self.cache.set(doc, embed)

        return [self.cache.get(doc) for doc in documents]

    def retrieve(self, documents: List[str], queries: List[str]):
        doc_embeds = np.array(self._retrieve_embeddings(documents))
        query_embeds = np.array(self._retrieve_embeddings(queries))

        cosine_scores = cos_sim(doc_embeds, query_embeds)

        closest_idxes: List[int] = cosine_scores.argmax(axis=0)
        return [documents[idx] for idx in closest_idxes]
