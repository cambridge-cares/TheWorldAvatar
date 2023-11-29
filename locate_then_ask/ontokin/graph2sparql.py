from typing import List
from locate_then_ask.graph2sparql import Graph2Sparql
from locate_then_ask.query_graph import QueryGraph


class OKGraph2Sparql(Graph2Sparql):
    def make_patterns_for_topic_entity_linking(
        self, query_graph: QueryGraph, topic_node: str
    ):
        label = query_graph.nodes[topic_node]["label"]
        if topic_node == "Reaction":
            template = '?{topic_entity} okin:hasEquation "{label}" .'
        elif topic_node == "Species":
            template = '?{topic_entity} skos:altLabel "{label}" .'
        else:
            template = '?{topic_entity} rdfs:label "{label}" .'

        return [template.format(topic_entity=topic_node, label=label)]

    def make_graph_pattern(self, query_graph: QueryGraph, s: str, o: str):
        if query_graph.nodes[o].get("template_node"):
            p = query_graph.edges[s, o]["label"]
            if p in ["ocape:hasReactant", "ocape:hasProduct", "(ocape:hasReactant|ocape:hasProduct)", "okin:hasGasPhase/^okin:belongsToPhase"]:
                assert query_graph.nodes[o]["rdf_type"] == "os:Species", query_graph.nodes[o]["rdf_type"]
                p_sparql = p + "/skos:altLabel"
            elif p == "okin:hasReaction":
                assert query_graph.nodes[o]["rdf_type"] == "okin:GasPhaseReaction"
                p_sparql = p + "/okin:hasEquation"
            else:
                return super().make_graph_pattern(query_graph, s, o)
            o_sparql = '"{label}"'.format(label=query_graph.nodes[o]["label"])
            s_sparql = self._resolve_node_to_sparql(query_graph, s)
            return "{s} {p} {o} .".format(s=s_sparql, p=p_sparql, o=o_sparql)
        else:
            return super().make_graph_pattern(query_graph, s, o)
