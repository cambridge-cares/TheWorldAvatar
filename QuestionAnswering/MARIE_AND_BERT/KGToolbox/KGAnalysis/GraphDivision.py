import json
import os
import pickle
import random

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
        self.graph_path = os.path.join(self.full_dataset_dir, "graph.pkl")
        self.graph = nx.Graph()
        self.stop_hash = ["bba36f226cdd988cfb4953ec680b214e315b120a0de4ba0fb12363487f7e1ad6",
                          "44bd7ae60f478fae1061e11a7739f4b94d1daf917982d33b6fc8a01a63f89c21",
                          "a9049af3804b328f93d89fc3d8882bd6b1bb4cebda43ace76bab2dbc080d084e",
                          "69ccf59b416bc5b36e3bce0b5bb9a8b04c4d5434ef56a18dce4d5aecd9c5ac9f",
                          "fc8842c641d9150647f928e6c294ce26dd51ecae5ba30b114a78ddedcd398815"]
        self.reaction_text_dict_path = os.path.join(self.full_dataset_dir, "ontokin_reactions_value_dict.json")
        self.reaction_text_dict = json.loads(open(self.reaction_text_dict_path).read())
        self.unique_reactions_path = os.path.join(self.full_dataset_dir, "unique_reaction_hash.json")

        if os.path.exists(self.unique_reactions_path):
            self.unique_reactions = json.loads(open(self.unique_reactions_path).read())
            # self.graph = pickle.load(open(self.graph_path))
        #
        # else:
        #     self.unique_reactions = self.create_graph()
        #     with open(self.unique_reactions_path, "w") as f:
        #         f.write(json.dumps(self.unique_reactions))
        #         f.close()

    def create_graph(self):
        """
        Iterate all the triples and create a nx Graph with undirected edges
        :return:
        """
        # also create a dictionary mapping reaction to the species idx it has ... using the graph .
        counter = 0
        unique_reactions = []
        for triple in self.triples:
            counter += 1
            print(f"{counter} out of {len(self.triples)}")
            s, p, o = [e.strip() for e in triple.split("\t")]
            s_idx = self.entity2idx[s]
            o_idx = self.entity2idx[o]
            # if p == "isReactant": # and o not in self.stop_hash:
            self.graph.add_node(s_idx)
            self.graph.add_node(o_idx)
            self.graph.add_edge(s_idx, o_idx)
            if o not in unique_reactions:
                unique_reactions.append(o)

        # pickle.dump(self.graph, open(self.graph_path, 'wb'))

        return unique_reactions

    """
    Find a way to compare the similarity between reactions ...
    Rule 1, there is a main embedding for a species, where the subgraph contains all the reactions the 
    species takes part in 
    However, those reactions interact with other species 
    How do you force the embedding of reactions consistent with other embedding spaces ...
    find the two most ridiculous species first, 
    G1: s1 -> all reactions 
    G2: s2 -> all reactions 
    There must be some overlapping between G1 and G2 ,"""

    def divide_graph_by_reaction_similarity(self):
        """
        calculate the string similarity (based on Levenshtein distance) between all reactions
        form a M x M matrix, then use dbscan to come up with the clustering of reactions ...
        :return:
        """
        reaction_text_list = []
        for reaction in self.unique_reactions:
            reaction_text = self.reaction_text_dict[reaction]
            reaction_text_list.append(reaction_text)

        with open(os.path.join(self.full_dataset_dir, "reaction_text_list.json"), "w") as f:
            f.write(json.dumps(reaction_text_list))
            f.close()

        # return reaction_text_list

    def divide_graph_by_species(self):
        s1 = self.entity2idx[self.stop_hash[0]]
        s2 = self.entity2idx[self.stop_hash[1]]
        G1 = nx.path_graph(s1)
        G2 = nx.path_graph(s2)

        # calculate the overlapping nodes between G1 and G2

    def make_reaction_species_idx_mapping(self):
        # selected_species = random.sample(self.unique_reactions, 500)
        reaction_species_mapping = {}
        unique_species = []
        counter = 0

        s_r_properties = ["isReactant", "isProduct"]
        r_s_properties = ["hasReactant", "hasProduct"]

        for triple in self.triples:
            counter += 1
            print(f"{counter} out of {len(self.triples)}")
            s, p, o = [e.strip() for e in triple.split("\t")]
            if p in s_r_properties:
                # if s not in unique_species:
                unique_species.append(s)
        unique_species = list(set(unique_species))
        selected_species = unique_species
        # selected_species = random.sample(unique_species, 1000)

        counter = 0
        for triple in self.triples:
            counter += 1
            print(f"{counter} out of {len(self.triples)}")
            s, p, o = [e.strip() for e in triple.split("\t")]
            s_idx = self.entity2idx[s]
            o_idx = self.entity2idx[o]
            if p in s_r_properties:
                if o_idx in reaction_species_mapping:
                    reaction_species_mapping[o_idx].append(s_idx)
                    reaction_species_mapping[o_idx] = list(set(reaction_species_mapping[o_idx]))
                else:
                    reaction_species_mapping[o_idx] = [s_idx]

        return reaction_species_mapping

        # with open(os.path.join(self.full_dataset_dir, "reaction_species_mapping_clean.json"), "w") as f:
        #     f.write(json.dumps(reaction_species_mapping))
        #     f.close()
        # with open(os.path.join(self.full_dataset_dir, "reaction_list_clean.json"), "w") as f:
        #     f.write(json.dumps(list(reaction_species_mapping.keys())))
        #     f.close()

    def find_subgraphs(self):
        idx_list = list(self.idx2entity.keys())
        _s = random.choice(idx_list)
        _t = random.choice(idx_list)
        m_c = nx.minimum_node_cut(self.graph)
        print(m_c)

        # d = sorted(list(nx.connected_components(self.graph)))
        # d = sorted(map(sorted, nx.k_edge_subgraphs(self.graph, k=2)))
        # for d_graph in d:
        #     print(len(d_graph))

    def run(self):
        self.make_reaction_species_idx_mapping()
        # self.divide_graph_by_reaction_similarity()
        # self.create_graph()
        # self.find_subgraphs()
        # nx.draw(self.graph, with_labels=True)
        # plt.show()


if __name__ == "__main__":
    my_divider = GraphDivider()
    my_divider.run()
