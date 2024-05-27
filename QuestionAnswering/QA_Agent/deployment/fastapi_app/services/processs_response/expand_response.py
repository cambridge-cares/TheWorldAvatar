from typing import Dict, Protocol

from constants.prefixes import TWA_ABOX_PREFIXES
from controllers.qa.model import TableDataItem
from services.kg import KgClient


class NodeDataGetter(Protocol):
    def __call__(self, iri: str, kg_client: KgClient) -> dict: ...


class SparqlResponseExpander:
    def __init__(self, kg_client: KgClient, type2getter: Dict[str, NodeDataGetter]):
        self.kg_client = kg_client
        self.type2getter = type2getter

    def get_types(self, iri: str):
        if not any(iri.startswith(prefix) for prefix in TWA_ABOX_PREFIXES):
            return []

        query = """SELECT DISTINCT ?Type
WHERE {{
    <{IRI}> a ?Type .
}}""".format(
            IRI=iri
        )

        return [
            row["Type"]["value"]
            for row in self.kg_client.query(query)["results"]["bindings"]
        ]

    def expand(self, item: TableDataItem):
        vars_set = set(item.vars)

        for binding in item.bindings:
            new_kv: Dict[str, dict] = dict()

            for key, val in binding.items():
                if not isinstance(val, str):
                    print("AAAAAAAAAAA")
                    print(val)
                    continue

                types = self.get_types(iri=val)

                for type in types:
                    data_getter = self.type2getter.get(type)

                    if not data_getter:
                        continue

                    node_data = data_getter(iri=val, kg_client=self.kg_client)

                    new_var = key + "Data"
                    if new_var not in vars_set:
                        item.vars.insert(item.vars.index(key) + 1, new_var)
                        vars_set.add(new_var)

                    new_kv[new_var] = node_data

            binding.update(new_kv)
