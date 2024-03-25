import itertools
from typing import Iterable

from redis import Redis


from .node_store import NodeDoc, NodeStore
from services.core.kg import KgClient
from services.core.embed import IEmbedder
from model.qa import QAData
from services.core.retrieve_docs import DocsRetriever


class RetrievalAgent:
    def __init__(self, embedder: IEmbedder, redis_client: Redis, node_docs_lst: Iterable[Iterable[NodeDoc]]):
        self.retriever = DocsRetriever(
            embedder=embedder,
            redis_client=redis_client,
            key="triples",
            docs=itertools.chain(node_docs_lst),
            linearize_func=NodeStore.linearize_doc,
        )

    def retrieve(self, query: str):
        retrieved = self.retriever.retrieve(
            queries=[query],
            k=3,
        )[0]
        retrieved = [dict(obj) for obj, _ in retrieved]
        return QAData(
            vars=["coarsely_retrieved_data"],
            bindings=[dict(coarsely_retrieved_data=data) for data in retrieved],
        )

def get_retrieval_agent(
    
):
    pass