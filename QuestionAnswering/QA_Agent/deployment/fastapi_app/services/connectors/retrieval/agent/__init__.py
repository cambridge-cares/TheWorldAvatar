from dataclasses import asdict
import itertools
from typing import Annotated, Iterable, List

from fastapi import Depends
from redis import Redis



from model.qa import QAData
from services.core.redis import get_redis_client
from services.core.retrieve_docs import DocsRetriever
from services.core.kg import KgClient
from services.core.embed import IEmbedder, get_embedder
from services.connectors.sg_land_lots.kg import get_sgLandLots_bgClient
from .node_store import NodeDoc, NodeStore


class RetrievalAgent:
    def __init__(
        self,
        embedder: IEmbedder,
        redis_client: Redis,
        node_docs_lst: Iterable[Iterable[NodeDoc]],
    ):
        self.retriever = DocsRetriever(
            embedder=embedder,
            redis_client=redis_client,
            key="triples",
            docs=itertools.chain(*node_docs_lst),
            linearize=NodeStore.linearize_doc,
            jsonify=asdict
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


def get_kgClients_toEmbed(
    sgLandLots_bgClient: Annotated[KgClient, Depends(get_sgLandLots_bgClient)],
):
    return [sgLandLots_bgClient]


def get_retrieval_agent(
    embedder: Annotated[IEmbedder, Depends(get_embedder)],
    redis_client: Annotated[Redis, Depends(get_redis_client)],
    kg_clients: Annotated[List[KgClient], Depends(get_kgClients_toEmbed)],
):
    return RetrievalAgent(
        embedder=embedder,
        redis_client=redis_client,
        node_docs_lst=[NodeStore(kg_client).triples_gen() for kg_client in kg_clients],
    )
