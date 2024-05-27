import itertools
from typing import Dict, List, Protocol, Union

from controllers.qa.model import TableDataItem
from services.kg import KgClient
from utils.rdf import flatten_sparql_response


class SparqlMakerFromIRI(Protocol):
    def __call__(self, IRI: str) -> str: ...


class SparqlResponseExpander:
    def __init__(
        self, bg_client: KgClient, type2sparql: Dict[str, SparqlMakerFromIRI]
    ):
        self.bg_client = bg_client
        self.type2sparql = type2sparql

    def get_types(self, iri: str):
        query = """SELECT ?Type
WHERE {{
    <{IRI}> a/rdfs:subClassOf* ?Type .
}}""".format(
            IRI=iri
        )

        return [
            row["Type"]["value"]
            for row in self.bg_client.query(query)["results"]["bindings"]
        ]

    def expand(self, item: TableDataItem):
        vars_set = set(item.vars)
        new_bindings: List[Dict[str, Union[str, float]]] = []

        for binding in item.bindings:
            new_kvs: List[List[dict]] = []

            for key, val in binding.items():
                types = set(self.get_types(iri=val))

                for type, sparql_maker in self.type2sparql.items():
                    if type in types:
                        query = sparql_maker(IRI=val)
                        res = self.bg_client.query(query)
                        node_vars, node_bindings = flatten_sparql_response(res)

                        new_var_insertion_idx = item.vars.index(key) + 1
                        for v in node_vars:
                            new_var = key + v
                            if new_var not in vars_set:
                                item.vars.insert(new_var_insertion_idx, new_var)
                                vars_set.add(new_var)
                                new_var_insertion_idx += 1

                        new_kvs.append(node_bindings)

            for combi in itertools.product(*new_kvs):
                new_binding = dict(binding)
                for kv in combi:
                    new_binding.update(kv)
                new_bindings.append(new_binding)

        item.bindings = new_bindings
