import os

import networkx as nx
import matplotlib.pyplot as plt

from Marie.Util.CommonTools.FileLoader import FileLoader
from Marie.Util.location import DATA_DIR


class GraphDivider:

    def __init__(self):
        self.dataset_dir = "CrossGraph/ontokin_reactions"
        self.ontology = "ontokin_reactions"
        self.full_dataset_dir = os.path.join(DATA_DIR, self.dataset_dir)
        self.file_loader = FileLoader(full_dataset_dir=self.full_dataset_dir,
                                      dataset_name=self.ontology)
        self.triples = self.file_loader.load_all_triples()
        self.entity2idx, self.idx2entity, self.rel2idx, self.idx2rel = self.file_loader.load_index_files()
        self.graph_path = os.path.join(self.full_dataset_dir, "graph.json")
        self.graph = nx.Graph()
        self.stop_hash = ["bba36f226cdd988cfb4953ec680b214e315b120a0de4ba0fb12363487f7e1ad6",
                          "44bd7ae60f478fae1061e11a7739f4b94d1daf917982d33b6fc8a01a63f89c21",
                          "a9049af3804b328f93d89fc3d8882bd6b1bb4cebda43ace76bab2dbc080d084e",
                          "69ccf59b416bc5b36e3bce0b5bb9a8b04c4d5434ef56a18dce4d5aecd9c5ac9f",
                          "fc8842c641d9150647f928e6c294ce26dd51ecae5ba30b114a78ddedcd398815"]

    def create_graph(self):
        """
        Iterate all the triples and create a nx Graph with undirected edges
        :return:
        """
        counter = 0
        for triple in self.triples:
            counter += 1
            print(f"{counter} out of {len(self.triples)}")
            s, p, o = [e.strip() for e in triple.split("\t")]
            s_idx = self.entity2idx[s]
            o_idx = self.entity2idx[o]
            if p == "isReactant" and o not in self.stop_hash:
                self.graph.add_node(s_idx)
                self.graph.add_node(o_idx)
                self.graph.add_edge(s_idx, o_idx)

    def find_subgraphs(self):
        # d = sorted(list(nx.connected_components(self.graph)))
        d = sorted(map(sorted, nx.k_edge_subgraphs(self.graph, k=2)))
        for d_graph in d:
            print(len(d_graph))

    def run(self):
        self.create_graph()
        self.find_subgraphs()
        # nx.draw(self.graph, with_labels=True)
        # plt.show()


if __name__ == "__main__":
    my_divider = GraphDivider()
    my_divider.run()
