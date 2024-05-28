from functools import cache
from typing import Annotated, List, Tuple

from fastapi import Depends

from services.example_store.model import TypedVarNode
from services.model import TableDataItem
from services.processs_response.augment_node import NodeDataRetriever
from services.processs_response.ontocompchem import get_ontocompchem_nodeDataRetriever
from services.processs_response.ontokin import get_ontokin_nodeDataRetriever
from services.processs_response.ontospecies import get_ontospecies_nodeDataRetriever


class TableAugmenter:
    def __init__(self, retrievers: Tuple[NodeDataRetriever, ...]):
        self.type2retriever = {
            type: retriever
            for retriever in retrievers
            for type in retriever.type2getter
        }

    def _retrieve(self, type: str, iris: List[str]):
        retriever = self.type2retriever.get(type)
        if not retriever:
            return [{} for _ in iris]
        return retriever.retrieve(type=type, iris=iris)

    def augment(self, nodes_to_augment: List[TypedVarNode], item: TableDataItem):
        vars_set = set(item.vars)

        for node in nodes_to_augment:
            if node.var not in vars_set:
                continue

            iris = [binding.get(node.var) for binding in item.bindings]
            data = self._retrieve(type=node.cls, iris=iris)

            if not any(data):
                continue

            new_var = node.var + "Data"
            item.vars.insert(item.vars.index(node.var) + 1, new_var)
            for binding, datum in zip(item.bindings, data):
                if datum:
                    binding[new_var] = datum


@cache
def get_table_augmenter(
    ontospecies_retriever: Annotated[
        NodeDataRetriever, Depends(get_ontospecies_nodeDataRetriever)
    ],
    ontokin_retriever: Annotated[
        NodeDataRetriever, Depends(get_ontokin_nodeDataRetriever)
    ],
    ontocompchem_retriever: Annotated[
        NodeDataRetriever, Depends(get_ontocompchem_nodeDataRetriever)
    ],
):
    return TableAugmenter(
        retrievers=(ontospecies_retriever, ontokin_retriever, ontocompchem_retriever)
    )
