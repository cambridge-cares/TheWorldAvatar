import networkx as nx

from constants.functions import (
    AROUND,
    EQUAL,
    GREATER_THAN,
    GREATER_THAN_EQUAL,
    INSIDE,
    LESS_THAN,
    LESS_THAN_EQUAL,
    OUTSIDE,
)
from locate_then_ask.graph2sparql import Graph2Sparql
from locate_then_ask.query_graph import QueryGraph


class OSGraph2Sparql(Graph2Sparql):
    def __init__(self):
        super().__init__()

    def make_topic_entity_patterns(self, query_graph: QueryGraph, topic_node: str):
        if query_graph.nodes[topic_node].get("template_node"):
            value_bindings = " ".join(
                [
                    '"{label}"'.format(label=name)
                    for name in query_graph.nodes["Species"]["label"]
                ]
            )
            return ["VALUES ?Species {{ {bindings} }}".format(bindings=value_bindings)]
        else:
            return []

    def make_graph_pattern(self, query_graph: QueryGraph, s: str, o: str):
        p = query_graph.edges[s, o]["label"]
        if p == "func":
            operand_left = "?" + s
            operator = query_graph.nodes[o]["operator"]
            operand_right = query_graph.nodes[o]["operand"]

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
                    return "FILTER ( {left} > -0.1 && {left} < 0.1 )".format(
                        left=operand_left
                    )
            elif operator == OUTSIDE:
                assert isinstance(operand_right, tuple)
                assert len(operand_right) == 2
                low, high = operand_right
                return "FILTER ( {left} < {low} || {left} > {high} )".format(
                    left=operand_left, low=low, high=high
                )
            else:
                raise ValueError("Unrecognized operator: " + operator)
        else:
            return super().make_graph_pattern(query_graph, s, o)
