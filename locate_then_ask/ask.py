import networkx as nx


class Asker:
    def __init__(self):
        pass

    def ask_query_name(self, query_graph: nx.DiGraph, verbalization: str):
        query_graph = query_graph.copy()
        query_graph.nodes["Species"]["question_node"] = True

        verbalization = "What is " + verbalization

        return query_graph, verbalization
    
    def ask_count(self, query_graph: nx.DiGraph, verbalization: str):
        query_graph = query_graph.copy()
        query_graph.nodes["Species"]["question_node"] = True
        query_graph.add_node("Species_func", label="count", func=True, template_node=True)
        query_graph.add_edge("Species", "Species_func")

        verbalization = "How many " + verbalization

        return query_graph, verbalization