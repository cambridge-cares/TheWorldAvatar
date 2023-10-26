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
    IDENTIFIER_KEYS,
    PROPERTY_KEYS,
    USE_KEY,
)
from locate_then_ask.sparql_compact2verbose import SparqlCompact2VerboseConverter


class GraphToSparqlConverter:
    def __init__(self):
        self.compact2verbose = SparqlCompact2VerboseConverter()

    def make_graph_pattern(self, query_graph: nx.DiGraph, s: str, o: str):
        p = query_graph.edges[s, o]["label"]
        if p.startswith("os:has"):
            key = p[len("os:has") :]
            if key in PROPERTY_KEYS or key in IDENTIFIER_KEYS:
                return "{s} {p} {o} .".format(s="?" + s, p=p, o="?" + key)
            elif key in [USE_KEY, CHEMCLASS_KEY]:
                if query_graph.nodes[o].get("template_node"):
                    literal = query_graph.nodes[o]["label"]
                    return "{s} {p} {o} .".format(
                        s="?" + s, p=p, o='"{literal}"'.format(literal=literal)
                    )
                else:
                    return "{s} {p} {o} .".format(s="?" + s, p=p, o="?" + key)
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
                    return "FILTER ( {left} > -0.1 && {left} < 0.1 )".format(
                        left=operand_left
                    )
            else:
                raise ValueError("Unrecognized operator: " + operator)
        else:
            raise ValueError("Unrecognized predicate: " + p)

    def make_select_clause(self, query_graph: nx.DiGraph):
        question_node = next(
            n
            for n, question_node in query_graph.nodes(data="question_node")
            if question_node
        )
        return "SELECT ?" + question_node

    def make_where_clause(self, query_graph: nx.DiGraph):
        if query_graph.nodes["Species"].get("template_node"):
            graph_patterns = [
                'VALUES ?Species {{ {bindings} }}'.format(
                    bindings = " ".join(['"{label}"'.format(label=name) for name in query_graph.nodes["Species"]["label"]])
                )
            ]
        else:
            graph_patterns = []

        graph_patterns.extend(
            [
                self.make_graph_pattern(query_graph, s, o)
                for s, o in nx.dfs_edges(query_graph, "Species")
            ]
        )
        return "WHERE {{\n  {group_graph_pattern}\n}}".format(
            group_graph_pattern="\n  ".join(graph_patterns)
        )

    def convert(self, query_graph: nx.DiGraph):
        select_clause = self.make_select_clause(query_graph)
        where_clause = self.make_where_clause(query_graph)

        sparql_compact = "{SELECT} {WHERE}".format(
            SELECT=select_clause, WHERE=where_clause
        )
        sparql_verbose = self.compact2verbose.convert(sparql_compact)

        return sparql_compact, sparql_verbose
