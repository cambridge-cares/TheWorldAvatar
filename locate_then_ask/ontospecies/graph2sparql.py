from locate_then_ask.graph2sparql import Graph2Sparql
from locate_then_ask.query_graph import QueryGraph


class OSGraph2Sparql(Graph2Sparql):
    def __init__(self):
        super().__init__()

    def make_patterns_for_topic_entity_linking(self, query_graph: QueryGraph, topic_node: str):
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
