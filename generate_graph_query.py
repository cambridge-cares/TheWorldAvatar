import random
from typing import Dict, List, Optional

import networkx as nx

from utils import Utils


class GraphQueryGenerator:
    def __init__(self, ontology: nx.DiGraph, prop_blacklist: List[str] = []):
        G = Utils.flatten_subclassof(ontology)
        G = Utils.remove_egdes_by_label(G, labels=prop_blacklist)
        self.ontology = G

    def generate_query_template(
        self,
        edge_num: int,
        question_node: Optional[str] = None,
    ):
        if question_node is None:
            question_node = random.choices(
                list(self.ontology.nodes()), weights=[self.ontology.degree[n] for n in self.ontology.nodes()]
            )[0]

        G = nx.MultiDiGraph()
        G.add_node(question_node, question_node=1)
        for _ in range(edge_num):
            n = random.choices(
                list(G.nodes()), weights=[int(self.ontology.degree[n] > 0) for n in G.nodes()]
            )[0]
            print("Chosen node: ", n)

            out_edges = list(self.ontology.out_edges(n, data="label"))
            in_edges = list(self.ontology.in_edges(n, data="label"))

            edge = random.choice(out_edges + in_edges)
            G.add_edge(edge[0], edge[1], label=edge[2])

        return G

    def ground_query_template(self):
        pass

    def generate_graph_query(self):
        query_template = self.generate_query_template()
        graph_query = self.ground_query_template()
        return graph_query
