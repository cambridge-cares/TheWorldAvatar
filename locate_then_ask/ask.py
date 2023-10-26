import copy
import random

import networkx as nx
from constants.functions import (
    AROUND,
    EQUAL,
    GREATER_THAN,
    GREATER_THAN_EQUAL,
    INSIDE,
    LESS_THAN,
    LESS_THAN_EQUAL,
)

from constants.ontospecies_keys import (
    CHEMCLASS_KEY,
    KEY2LABELS,
    PROPERTY_KEYS,
    SPECIES_ATTRIBUTE_KEYS,
    USE_KEY,
)


class Asker:
    def __init__(self):
        pass

    def query_graph_to_sparql(self, query_graph: nx.DiGraph):
        question_node = next(
            n
            for n, question_node in query_graph.nodes(data="question_node")
            if question_node
        )
        select_clause = "SELECT ?" + question_node

        if query_graph.nodes["Species"].get("template_node"):
            graph_patterns = [
                'VALUES ?Species {{ "{label}" }}'.format(label=query_graph.nodes["Species"]["label"])
            ]
        else:
            graph_patterns = []

        def make_graph_pattern(s: str, o: str):
            p = query_graph.edges[s, o]["label"]
            if p.startswith("os:has"):
                key = p[len("os:has") :]
                if key in PROPERTY_KEYS:
                    return "{s} {p} {o}".format(s="?" + s, p=p, o="?" + key)
                elif key in [USE_KEY, CHEMCLASS_KEY]:
                    literal = query_graph.nodes[o]["label"]
                    return "{s} {p} {o}".format(
                        s="?" + s, p=p, o='"{literal}"'.format(literal=literal)
                    )
                else:
                    raise ValueError("Unrecognized predicate: " + p)
            elif p == "func":
                in_edges = query_graph.in_edges(s, data="label")
                assert len(in_edges) == 1
                _, _, predicate = next(iter(in_edges))
                assert predicate.startswith("os:has")
                key = predicate[len("os:has") :]

                operand_left = "?" + key
                operand_right = query_graph.nodes[s]["label"]
                operator = query_graph.nodes[o]["label"]
                if operator in [
                    LESS_THAN,
                    GREATER_THAN,
                    LESS_THAN_EQUAL,
                    GREATER_THAN_EQUAL,
                    EQUAL,
                ]:
                    return "FILTER ( {left} {op} {right} )".format(
                        left=operand_left, op=operator, right=operand_right
                    )
                elif operator == INSIDE:
                    assert isinstance(operand_right, tuple)
                    assert len(operand_right) == 2
                    low, high = operand_right
                    return "FILTER ( {left} > {low} && {left} < {high} )".format(
                        left=operand_left, low=low, high=high
                    )
                elif operator == AROUND:
                    if operand_right < 0:
                        return "FILTER ( {left} > {right}*1.1 && {left} < {right}*0.9 )".format(
                            left=operand_left, right=operand_right
                        )
                    elif operand_right > 0:
                        return "FILTER ( {left} > {right}*0.9 && {left} < {right}*1.1 )".format(
                            left=operand_left, right=operand_right
                        )
                    else:
                        return "FILTER ( {left} > -0.1 && {right} < 0.1 )".format(
                            left=operand_left
                        )
                else:
                    raise ValueError("Unrecognized operator: " + operator)
            else:
                raise ValueError("Unrecognized predicate: " + p)

        graph_patterns.extend([
            make_graph_pattern(s, o) for s, o in nx.dfs_edges(query_graph, "Species")
        ])
        where_clause = "WHERE {{\n  {group_graph_pattern}\n}}".format(
            group_graph_pattern="\n  ".join(graph_patterns)
        )

        return "{SELECT} {WHERE}".format(SELECT=select_clause, WHERE=where_clause)

    def ask_query_name(self, query_graph: nx.DiGraph, verbalization: str):
        query_graph = copy.deepcopy(query_graph)
        query_graph.nodes["Species"]["question_node"] = True

        query_sparql = self.query_graph_to_sparql(query_graph)
        verbalization = "What is " + verbalization

        return query_graph, query_sparql, verbalization

    def ask_count(self, query_graph: nx.DiGraph, verbalization: str):
        query_graph = copy.deepcopy(query_graph)
        query_graph.nodes["Species"]["question_node"] = True
        query_graph.add_node(
            "Species_func", label="count", func=True, template_node=True
        )
        query_graph.add_edge("Species", "Species_func")

        query_sparql = self.query_graph_to_sparql(query_graph)
        verbalization = "How many " + verbalization

        return query_graph, query_sparql, verbalization

    def ask_query_attr(self, query_graph: nx.DiGraph, verbalization: str):
        key_sampling_frame = [
            x for x in SPECIES_ATTRIBUTE_KEYS if x not in query_graph.nodes()
        ]
        key = random.choice(key_sampling_frame)
        key_label = random.choice(KEY2LABELS[key])

        query_graph = copy.deepcopy(query_graph)
        query_graph.add_node(key, question_node=True)
        query_graph.add_edge("Species", key, label="os:has" + key)

        query_sparql = self.query_graph_to_sparql(query_graph)
        verbalization = "For {E}, what is its {K}".format(E=verbalization, K=key_label)

        return query_graph, query_sparql, verbalization
