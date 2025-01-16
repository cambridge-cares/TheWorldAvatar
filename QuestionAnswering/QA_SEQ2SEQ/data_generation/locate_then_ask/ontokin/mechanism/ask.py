from locate_then_ask.ask import ask_name
from locate_then_ask.query_graph import QueryGraph


class OKMechanismAsker:
    def ask_name(self, query_graph: QueryGraph, verbalization: str):
        return ask_name(query_graph, verbalization, "Mechanism")