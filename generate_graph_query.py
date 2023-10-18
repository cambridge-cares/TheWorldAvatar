import random
from typing import List, Optional

import networkx as nx

from utils import Utils


class GraphQueryGenerator:
    def __init__(
        self,
        ontology: nx.DiGraph,
        prop_blacklist: List[str] = [],
        questionnode_blacklist: List[str] = [],
    ):
        G = Utils.flatten_subclassof(ontology)
        G = Utils.remove_egdes_by_label(G, labels=prop_blacklist)
        self.ontology = G
        self.questionnode_blacklist = questionnode_blacklist

    def generate_query_template(
        self,
        edge_num: int,
        question_node: Optional[str] = None,
    ):
        if question_node is None:
            sampling_frame = [
                n for n in self.ontology.nodes() if n not in self.questionnode_blacklist
            ]
            question_node = random.choices(
                sampling_frame,
                weights=[self.ontology.degree[n] for n in sampling_frame],
            )[0]

        G = nx.MultiDiGraph()
        G.add_node(question_node, question_node=True)
        for _ in range(edge_num):
            n = random.choices(
                list(G.nodes()),
                weights=[int(self.ontology.degree[n] > 0) for n in G.nodes()],
            )[0]

            out_edges = list(self.ontology.out_edges(n, data="label"))
            in_edges = list(self.ontology.in_edges(n, data="label"))

            edge = random.choice(out_edges + in_edges)
            G.add_edge(edge[0], edge[1], label=edge[2])

        for n, question_node in G.nodes(data="question_node"):
            if question_node:
                continue
            if random.uniform(0, 1) < 0.8:
                G.nodes[n]["template_node"] = True

        return G

    def ground_query_template(self, query_template: nx.MultiDiGraph):
        pass

    def generate_graph_query(
        self, edge_num: int = 1, question_node: Optional[str] = None
    ):
        query_template = self.generate_query_template(
            edge_num=edge_num, question_node=question_node
        )
        graph_query = self.ground_query_template(query_template)
        return graph_query
