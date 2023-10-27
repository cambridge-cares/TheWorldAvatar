import copy
from dataclasses import dataclass
import random
from typing import Tuple

import networkx as nx

from constants.ontospecies_keys import (
    KEY2LABELS,
    SPECIES_ATTRIBUTE_KEYS,
)
from locate_then_ask.graph2sparql import GraphToSparqlConverter

@dataclass
class AskDatum:
    query_graph: nx.DiGraph
    query_sparql: Tuple[str, str]
    verbalization: str

class Asker:
    def __init__(self):
        self.graph2sparql = GraphToSparqlConverter()

    def ask_name(self, query_graph: nx.DiGraph, verbalization: str):
        query_graph = copy.deepcopy(query_graph)
        query_graph.nodes["Species"]["question_node"] = True

        query_sparql = self.graph2sparql.convert(query_graph)
        verbalization = "What are " + verbalization

        return AskDatum(
            query_graph=query_graph,
            query_sparql=query_sparql,
            verbalization=verbalization,
        )

    def ask_count(self, query_graph: nx.DiGraph, verbalization: str):
        query_graph = copy.deepcopy(query_graph)
        query_graph.nodes["Species"]["question_node"] = True
        query_graph.add_node(
            "Species_func", label="count", func=True, template_node=True
        )
        query_graph.add_edge("Species", "Species_func")

        query_sparql = self.graph2sparql.convert(query_graph)
        verbalization = "How many " + verbalization

        return AskDatum(
            query_graph=query_graph,
            query_sparql=query_sparql,
            verbalization=verbalization,
        )

    def ask_attribute(self, query_graph: nx.DiGraph, verbalization: str):
        sampled_keys = [
            p[len("os:has") :]
            for _, _, p in query_graph.edges(data="label")
            if p.startswith("os:has")
        ]
        key_sampling_frame = [
            x for x in SPECIES_ATTRIBUTE_KEYS if x not in sampled_keys
        ]
        key = random.choice(key_sampling_frame)
        key_label = random.choice(KEY2LABELS[key])

        query_graph = copy.deepcopy(query_graph)
        query_graph.add_node(key, question_node=True)
        query_graph.add_edge("Species", key, label="os:has" + key)

        query_sparql = self.graph2sparql.convert(query_graph)
        verbalization = "For {E}, what is its {K}".format(E=verbalization, K=key_label)

        return AskDatum(
            query_graph=query_graph,
            query_sparql=query_sparql,
            verbalization=verbalization,
        )
