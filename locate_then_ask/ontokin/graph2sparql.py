from typing import List
from locate_then_ask.graph2sparql import Graph2Sparql
from locate_then_ask.query_graph import QueryGraph


class OKGraph2Sparql(Graph2Sparql):
    def __init__(self):
        super().__init__(
            predicates_to_entities_linked_by_rdfslabel=[
                "ocape:hasReactant",
                "ocape:hasProduct",
                "okin:belongsToPhase/okin:containedIn",
            ]
        )

    def make_topic_entity_patterns(self, query_graph: QueryGraph, topic_node: str):
        label = query_graph.nodes[topic_node]["label"]
        if topic_node == "Reaction":
            template = '?{topic_entity} okin:hasEquation "{label}" .'
        else:
            template = '?{topic_entity} rdfs:label "{label}" .'

        return [template.format(topic_entity=topic_node, label=label)]
