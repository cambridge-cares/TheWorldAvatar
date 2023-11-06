from constants.functions import (
    AROUND,
    EQUAL,
    GREATER_THAN,
    GREATER_THAN_EQUAL,
    INSIDE,
    LESS_THAN,
    LESS_THAN_EQUAL,
)
from locate_then_ask.graph2sparql import Graph2Sparql
from locate_then_ask.query_graph import QueryGraph


class OSGraph2Sparql(Graph2Sparql):
    def make_graph_pattern(self, query_graph: QueryGraph, s: str, o: str):
        p = query_graph.edges[s, o]["label"]
        if p.startswith("os:has"):
            if p.endswith("/os:value"):
                key = p[len("os:has") : -len("/os:value")]
                return "{s} {p} {o} .".format(
                    s="?" + s, p=p, o="?{key}Value".format(key=key)
                )
            elif p.endswith("/rdfs:label"):
                key = p[len("os:has") : -len("/rdfs:label")]
                if query_graph.nodes[o].get("template_node"):
                    literal = query_graph.nodes[o]["label"]
                    return "{s} {p} {o} .".format(
                        s="?" + s, p=p, o='"{literal}"'.format(literal=literal)
                    )
                else:
                    return "{s} {p} {o} .".format(s="?" + s, p=p, o="?" + key)
            else:
                key = p[len("os:has"):]
                return "{s} {p} {o} .".format(s="?" + s, p=p, o="?" + key)
        elif p.startswith("?has"):
            return "{s} {p} {o} .".format(s="?" + s, p=p, o="?" + o)
        elif p == "func":
            in_edges = query_graph.in_edges(s, data="label")
            assert len(in_edges) == 1
            _, _, predicate = next(iter(in_edges))
            assert predicate.startswith("os:has")
            key = predicate.split("/")[0][len("os:has") :]

            operand_left = "?{key}Value".format(key=key)
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
            s = s if query_graph.nodes[s].get("template_node") else ("?" + s)
            o = o if query_graph.nodes[o].get("template_node") else ("?" + o)
            return "{s} {p} {o} .".format(s=s, p=p, o=o)

    def make_where_clause(self, query_graph: QueryGraph):
        if query_graph.nodes["Species"].get("template_node"):
            graph_patterns = [
                "VALUES ?Species {{ {bindings} }}".format(
                    bindings=" ".join(
                        [
                            '"{label}"'.format(label=name)
                            for name in query_graph.nodes["Species"]["label"]
                        ]
                    )
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
