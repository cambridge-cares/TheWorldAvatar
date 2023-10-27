import copy
from dataclasses import dataclass
import random
from typing import Tuple

import networkx as nx

from constants.ontospecies_keys import (
    CHEMCLASS_KEY,
    IDENTIFIER_KEYS,
    KEY2LABELS,
    PROPERTY_KEYS,
    SPECIES_ABSTRACT_ATTRIBUTE_KEYS,
    SPECIES_ATTRIBUTE_KEYS,
    USE_KEY,
)
from locate_then_ask.graph2sparql import GraphToSparqlConverter
from locate_then_ask.utils import get_attribute_keys


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

    def ask_attribute(
        self, query_graph: nx.DiGraph, verbalization: str, attr_num: int = 1
    ):
        query_graph = copy.deepcopy(query_graph)

        will_sample_concrete_attribute = random.sample(
            population=[True, False],
            counts=[len(SPECIES_ATTRIBUTE_KEYS), len(SPECIES_ABSTRACT_ATTRIBUTE_KEYS)],
            k=1,
        )[0]

        if will_sample_concrete_attribute:
            sampled_keys = get_attribute_keys(query_graph)
            key_sampling_frame = [
                x for x in SPECIES_ATTRIBUTE_KEYS if x not in sampled_keys
            ]
            keys = random.sample(
                key_sampling_frame, k=min(attr_num, len(key_sampling_frame))
            )
            keys_label = []

            for key in keys:
                if key in PROPERTY_KEYS + IDENTIFIER_KEYS:
                    obj = key
                    predicate = "os:has{Name}".format(Name=key)
                elif key in [USE_KEY, CHEMCLASS_KEY]:
                    obj = key + "Label"
                    predicate = "os:has{Name}/rdfs:label".format(Name=key)

                query_graph.add_node(obj, question_node=True)
                query_graph.add_edge("Species", obj, label=predicate)

                key_label = random.choice(KEY2LABELS[key])
                keys_label.append(key_label)

            query_sparql = self.graph2sparql.convert(query_graph)

            species_num = (
                1
                if not isinstance(query_graph.nodes["Species"]["label"], list)
                else len(query_graph.nodes["Species"]["label"])
            )

            template = "For {E}, what {be} {possessive_adj} {K}"
            verbalization = template.format(
                E=verbalization,
                be="are" if len(keys_label) > 1 else "is",
                possessive_adj="their" if species_num > 1 else "its",
                K=" and ".join(keys_label),
            )
        else:
            key = random.choice(SPECIES_ABSTRACT_ATTRIBUTE_KEYS)
            key_node = key + "Name"
            abstract_key_node = "os:" + key
            query_graph.add_nodes_from(
                [
                    (key_node, dict(question_node=True)),
                    (abstract_key_node, dict(template_node=True)),
                ]
            )
            query_graph.add_edge(
                "Species", key_node, label="?has{key}Name".format(key=key)
            )
            query_graph.add_edge(
                key_node, abstract_key_node, label="rdf:type/rdfs:subClassOf"
            )

            key_label = random.choice(KEY2LABELS[key])

            query_sparql = self.graph2sparql.convert(query_graph)
            verbalization = "For {E}, what are its {K}".format(
                E=verbalization, K=key_label
            )

        return AskDatum(
            query_graph=query_graph,
            query_sparql=query_sparql,
            verbalization=verbalization,
        )
