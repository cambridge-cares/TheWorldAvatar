from functools import cache
from typing import Annotated, List

from fastapi import Depends
import numpy as np

from services.utils.numerical import cosine_similarity
from services.embed import IEmbedder, get_embedder
from services.cache import ICache, RedisCache


# TODO: use a dedicated vector database and k-NN search engine
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
        if new_docs:
            new_embeds = self.embeder(new_docs)
            for doc, embed in zip(new_docs, new_embeds):
                self.cache.set(doc, embed)

        return np.array([self.cache.get(doc) for doc in documents])

    def retrieve(self, documents: List[str], queries: List[str]):
        # TODO: handle when `documents` or `queries` is an empty List
        # TODO: enable top-k retrieval
        doc_embeds = self._retrieve_embeddings(documents)
        query_embeds = self._retrieve_embeddings(queries)

        cosine_scores = cosine_similarity(doc_embeds, query_embeds)

        closest_idxes: List[int] = cosine_scores.argmax(axis=0)
        return [documents[idx] for idx in closest_idxes]


@cache
def get_nn_retriever(embedder: Annotated[IEmbedder, Depends(get_embedder)]):
    return NNRetriever(embedder=embedder, cache=RedisCache())
