from typing import List
from locate_then_ask.graph2sparql import Graph2Sparql
from locate_then_ask.query_graph import QueryGraph


class OKGraph2Sparql(Graph2Sparql):
    def __init__(self):
        super().__init__(
            predicates_to_entities_linked_by_rdfslabel=[
                "ocape:hasReactant",
                "ocape:hasProduct",
            ]
        )

    def make_patterns_for_topic_entity_linking(
        self, query_graph: QueryGraph, topic_node: str
    ):
        label = query_graph.nodes[topic_node]["label"]
        if topic_node == "Reaction":
            template = '?{topic_entity} okin:hasEquation "{label}" .'
        else:
            template = '?{topic_entity} rdfs:label "{label}" .'

        return [template.format(topic_entity=topic_node, label=label)]

    def make_graph_pattern(self, query_graph: QueryGraph, s: str, o: str):
        p = query_graph.edges[s, o]["label"]

        if query_graph.nodes[o].get("template_node") and p in [
            "okin:belongsToPhase/okin:containedIn",
            "^okin:containedIn/^okin:belongsToPhase",
        ]:
            obj_type = query_graph.nodes[o]["rdf_type"]
            if obj_type in ["okin:Species", "okin:ReactionMechanism"]:
                p_sparql = p + "/rdfs:label"
            elif obj_type == "okin:GasPhaseReaction":
                p_sparql = p + "/okin:hasEquation"
            else:
                raise Exception(
                    "Expects object type for predicate {p} to be either `okin:Species` or `okin:GasPhaseReaction`, but found: {o_type}. Subject is {s}.".format(
                        p=p, o_type=obj_type, s=s
                    )
                )
            o_sparql = '"{label}"'.format(label=query_graph.nodes[o]["label"])
            s_sparql = self._resolve_node_to_sparql(query_graph, s)
            return "{s} {p} {o} .".format(s=s_sparql, p=p_sparql, o=o_sparql)
        else:
            return super().make_graph_pattern(query_graph, s, o)
