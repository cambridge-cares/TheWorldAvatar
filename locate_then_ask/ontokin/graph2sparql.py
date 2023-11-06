from typing import List
from locate_then_ask.graph2sparql import Graph2Sparql
from locate_then_ask.query_graph import QueryGraph


class OKGraph2Sparql(Graph2Sparql):
    def __init__(self):
        super().__init__(predicates_to_entities_linked_by_rdfslabel=[
            "ocape:hasReactant",
            "ocape:hasProduct",
            "okin:belongsToPhase/okin:containedIn",
        ])