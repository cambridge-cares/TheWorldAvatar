import copy
from locate_then_ask.data_model import AskDatum
from locate_then_ask.ontokin.graph2sparql import OKGraph2Sparql
from locate_then_ask.query_graph import QueryGraph


class OKMechanismAsker:
    def __init__(self):
        self.graph2sparql = OKGraph2Sparql()

    def ask_name(self, query_graph: QueryGraph, verbalization: str):
        query_graph = copy.deepcopy(query_graph)
        query_graph.nodes["Mechanism"]["question_node"] = True

        query_sparql = self.graph2sparql.convert(query_graph)
        verbalization = "What is " + verbalization

        return AskDatum(
            query_graph=query_graph,
            query_sparql=query_sparql,
            verbalization=verbalization,
        )
