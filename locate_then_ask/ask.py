import copy
import random

import networkx as nx

from constants.ontospecies_keys import KEY2LABELS, SPECIES_ATTRIBUTE_KEYS


class Asker:
    def __init__(self):
        pass

    def ask_query_name(self, query_graph: nx.DiGraph, verbalization: str):
        query_graph = copy.deepcopy(query_graph)
        query_graph.nodes["Species"]["question_node"] = True

        verbalization = "What is " + verbalization

        return query_graph, verbalization
    
    def ask_count(self, query_graph: nx.DiGraph, verbalization: str):
        query_graph = copy.deepcopy(query_graph)
        query_graph.nodes["Species"]["question_node"] = True
        query_graph.add_node("Species_func", label="count", func=True, template_node=True)
        query_graph.add_edge("Species", "Species_func")

        verbalization = "How many " + verbalization

        return query_graph, verbalization
    
    def ask_query_attr(self, query_graph: nx.DiGraph, verbalization: str):
        key_sampling_frame = [x for x in SPECIES_ATTRIBUTE_KEYS if x not in query_graph.nodes()]
        key = random.choice(key_sampling_frame)
        key_label = random.choice(KEY2LABELS[key])

        query_graph = copy.deepcopy(query_graph)
        query_graph.add_node(key, question_node=True)
        query_graph.add_edge("Species", key, label="os:has" + key)

        template = "For {E}, what is its {K}"
        verbalization = template.format(E=verbalization, K=key_label)

        return query_graph, verbalization
