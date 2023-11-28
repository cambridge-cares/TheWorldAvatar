from typing import List
import networkx as nx
from constants.functions import NumOp, StrOp

from locate_then_ask.query_graph import QueryGraph


class Graph2Sparql:
    def __init__(self, predicates_to_entities_linked_by_rdfslabel: List[str] = []):
        self.predicates_to_entities_linked_by_rdfslabel = (
            predicates_to_entities_linked_by_rdfslabel
        )

    def _resolve_node_to_sparql(self, query_graph: QueryGraph, n: str):
        if query_graph.nodes[n].get("template_node"):
            if query_graph.nodes[n].get("topic_entity"):
                return "?" + n
            elif query_graph.nodes[n].get("literal"):
                return '"{label}"'.format(label=query_graph.nodes[n]["label"])
            elif not query_graph.nodes[n].get("prefixed"):
                return "<{iri}>".format(iri=query_graph.nodes[n]["iri"])
            else:
                return query_graph.nodes[n]["iri"]
        else:
            return "?" + n

    def _make_numerical_operator_pattern(self, query_graph: QueryGraph, s: str, o: str):
        operand_left = "?" + s
        operator = query_graph.nodes[o]["operator"]
        operand_right = query_graph.nodes[o]["operand"]

        if operator in [
            NumOp.LESS_THAN,
            NumOp.GREATER_THAN,
            NumOp.LESS_THAN_EQUAL,
            NumOp.GREATER_THAN_EQUAL,
            NumOp.EQUAL,
        ]:
            return "FILTER ( {left} {op} {right} )".format(
                left=operand_left, op=operator, right=operand_right
            )
        elif operator == NumOp.INSIDE_RANGE:
            assert isinstance(operand_right, tuple)
            assert len(operand_right) == 2
            low, high = operand_right
            return "FILTER ( {left} > {low} && {left} < {high} )".format(
                left=operand_left, low=low, high=high
            )
        elif operator == NumOp.AROUND:
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
        elif operator == NumOp.OUTSIDE_RANGE:
            assert isinstance(operand_right, tuple)
            assert len(operand_right) == 2
            low, high = operand_right
            return "FILTER ( {left} < {low} || {left} > {high} )".format(
                left=operand_left, low=low, high=high
            )
        else:
            raise ValueError("Unrecognized numerical operator: " + operator)

    def _make_string_operator_pattern(self, query_graph: QueryGraph, s: str, o: str):
        operand_left = "?" + s
        operator = query_graph.nodes[o]["operator"]
        values = query_graph.nodes[o]["operand"]
        if operator == StrOp.VALUES:
            return "VALUES {left} {{ {values} }}".format(
                left=operand_left,
                values=" ".join(['"{val}"'.format(val=x) for x in values]),
            )
        else:
            raise ValueError("Unrecognized string operator: " + operator)

    def make_graph_pattern(self, query_graph: QueryGraph, s: str, o: str):
        s_sparql = self._resolve_node_to_sparql(query_graph, s)
        p = query_graph.edges[s, o]["label"]

        if p == "func":
            operator = query_graph.nodes[o]["operator"]
            if isinstance(operator, NumOp):
                return self._make_numerical_operator_pattern(query_graph, s, o)
            elif isinstance(operator, StrOp):
                return self._make_string_operator_pattern(query_graph, s, o)
            else:
                raise ValueError("Unexpected operator: " + str(operator))
        if (
            query_graph.nodes[o].get("template_node")
            and p in self.predicates_to_entities_linked_by_rdfslabel
        ):
            p_sparql = p + "/rdfs:label"
            o_sparql = '"{label}"'.format(label=query_graph.nodes[o]["label"])
        else:
            p_sparql = p
            o_sparql = self._resolve_node_to_sparql(query_graph, o)

        return "{s} {p} {o} .".format(s=s_sparql, p=p_sparql, o=o_sparql)

    def make_patterns_for_topic_entity_linking(
        self, query_graph: QueryGraph, topic_node: str
    ) -> List[str]:
        return []

    def make_where_clause(self, query_graph: QueryGraph):
        topic_node = next(
            n
            for n, topic_entity in query_graph.nodes(data="topic_entity")
            if topic_entity
        )

        graph_patterns = []
        if query_graph.nodes[topic_node].get("template_node"):
            graph_patterns.extend(
                self.make_patterns_for_topic_entity_linking(query_graph, topic_node)
            )

        for s, o in nx.edge_dfs(query_graph, topic_node):
            graph_patterns.append(self.make_graph_pattern(query_graph, s, o))

        return "WHERE {{\n  {group_graph_pattern}\n}}".format(
            group_graph_pattern="\n  ".join(graph_patterns)
        )

    def make_select_clause(self, query_graph: QueryGraph):
        question_nodes = [
            n
            for n, question_node in query_graph.nodes(data="question_node")
            if question_node
        ]

        def resolve_proj(n: str):
            if query_graph.nodes[n].get("ask_count"):
                return "(COUNT(?{n}) AS ?{n}Count)".format(n=n)
            return "?" + n

        return "SELECT " + " ".join([resolve_proj(n) for n in question_nodes])

    def convert(self, query_graph: QueryGraph):
        select_clause = self.make_select_clause(query_graph)
        where_clause = self.make_where_clause(query_graph)

        sparql_compact = "{SELECT} {WHERE}".format(
            SELECT=select_clause, WHERE=where_clause
        )

        return sparql_compact
