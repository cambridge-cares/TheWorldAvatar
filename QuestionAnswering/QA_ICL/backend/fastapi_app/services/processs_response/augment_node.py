from collections import defaultdict
from typing import Any, Dict, List, Protocol, Sequence
from services.kg import KgClient
from utils.collections import FrozenDict


class NodeDataGetter(Protocol):
    def __call__(self, kg_client: KgClient, iris: Sequence[str]) -> Sequence[dict]: ...


class NodeDataRetriever:
    def __init__(self, kg_client: KgClient, type2getter: Dict[str, NodeDataGetter]):
        self.kg_client = kg_client
        self.type2getter = type2getter

    def retrieve(self, type: str, iris: List[Any]):
        data = [FrozenDict({}) for _ in iris]
        getter = self.type2getter.get(type)

        if not getter:
            return data

        iri2idxes = defaultdict(list)
        for idx, iri in enumerate(iris):
            if not isinstance(iri, str):
                continue
            iri2idxes[iri].append(idx)

        str_iris = iri2idxes.keys()
        iri2dataidx = {iri: i for i, iri in enumerate(str_iris)}
        node_data = [
            FrozenDict.from_dict(obj)
            for obj in getter(kg_client=self.kg_client, iris=str_iris)
        ]

        for iri, idxes in iri2idxes.items():
            for idx in idxes:
                data[idx] = node_data[iri2dataidx[iri]]

        return data
