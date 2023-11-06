from typing import List
import networkx as nx

from locate_then_ask.query_graph import QueryGraph


class Graph2Sparql:
    def __init__(self, predicates_to_entities_linked_by_rdfslabel: List[str] = []):
        self.predicates_to_entities_linked_by_rdfslabel = (
            predicates_to_entities_linked_by_rdfslabel
        )

    def _resolve_node_to_sparql(self, query_graph: QueryGraph, n: str):
        if query_graph.nodes[n].get("template_node"):
            if query_graph.nodes[n].get("literal"):
                return '"{label}"'.format(label=query_graph.nodes[n]["label"])
            elif not query_graph.nodes[n].get("prefixed"):
                return "<{iri}>".format(iri=query_graph.nodes[n]["iri"])
            else:
                return query_graph.nodes[n]["iri"]
        else:
            return "?" + n

    def make_graph_pattern(self, query_graph: QueryGraph, s: str, o: str):
        s_sparql = self._resolve_node_to_sparql(query_graph, s)
        p = query_graph.edges[s, o]["label"]

        if p in self.predicates_to_entities_linked_by_rdfslabel and query_graph.nodes[
            o
        ].get("template_node"):
            p_sparql = p + "/rdfs:label"
            o_sparql = '"{label}"'.format(label=query_graph.nodes[o]["label"])
        else:
            p_sparql = p
            o_sparql = self._resolve_node_to_sparql(query_graph, o)

        return "{s} {p} {o} .".format(s=s_sparql, p=p_sparql, o=o_sparql)

    def make_where_clause(self, query_graph: QueryGraph):
        topic_node = next(
            n
            for n, topic_entity in query_graph.nodes(data="topic_entity")
            if topic_entity
        )
        graph_patterns = [
            self.make_graph_pattern(query_graph, s, o)
            for s, o in nx.dfs_edges(query_graph, topic_node)
        ]
        return "WHERE {{\n  {group_graph_pattern}\n}}".format(
            group_graph_pattern="\n  ".join(graph_patterns)
        )

    def make_select_clause(self, query_graph: QueryGraph):
        question_nodes = [
            n
            for n, question_node in query_graph.nodes(data="question_node")
            if question_node
        ]
        return "SELECT " + " ".join(["?" + n for n in question_nodes])

    def convert(self, query_graph: QueryGraph):
        select_clause = self.make_select_clause(query_graph)
        where_clause = self.make_where_clause(query_graph)

        sparql_compact = "{SELECT} {WHERE}".format(
            SELECT=select_clause, WHERE=where_clause
        )

        return sparql_compact
