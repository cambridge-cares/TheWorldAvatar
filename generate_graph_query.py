import random
from typing import List, Optional, Tuple

import networkx as nx

from constants import RDFS_SUBCLASSOF


class GraphQueryGenerator:
    def __init__(self, ontology: nx.DiGraph):
        self.ontology = ontology
        self.degree = dict(ontology.degree)

    def get_prop2path(self, s: str):
        prop2path = dict()

        def dfs(n: str, edge_path: Tuple[str]):
            for e in self.ontology.out_edges(n, data="label"):
                h, t, prop = e
                if prop != RDFS_SUBCLASSOF:
                    prop2path[prop] = edge_path + (e,)
                else:
                    dfs(t, edge_path + (e,))

        dfs(s, tuple())
        return prop2path

    def generate_query_template(
        self,
        edge_num: int,
        question_node: Optional[str] = None,
        prop_blacklist: List[str] = [],
    ):
        if question_node is None:
            question_node = random.choices(
                list(self.degree.keys()), weights=self.degree.values()
            )[0]

        G = nx.MultiDiGraph()
        G.add_node(question_node, question_node=1)
        for i in range(edge_num):
            n = random.choices(
                list(G.nodes), weights=[int(self.degree[n] > 0) for n in G.nodes]
            )[0]
            
            prop2path = self.get_prop2path(n)
            if i == 0 and n == question_node:
                in_edges = list(self.ontology.in_edges(n, data="label"))
            else:
                in_edges = []

            prop = random.choice(
                [
                    x
                    for x in list(prop2path.keys()) + [x[2] for x in in_edges]
                    if x not in prop_blacklist
                ]
            )
            if prop in prop2path:
                path = prop2path[prop]
                G.add_edge(path[0][0], path[-1][1], label=prop, path=path)
            else:
                edge = random.choice([x for x in in_edges if x[2] == prop])
                G.add_edge(edge[0], edge[1], label=edge[2])

        return G

    def ground_query_template(self):
        pass

    def generate_graph_query(self):
        query_template = self.generate_query_template()
        graph_query = self.ground_query_template()
        return graph_query
