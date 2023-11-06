from locate_then_ask.graph2sparql import Graph2Sparql
from locate_then_ask.query_graph import QueryGraph


class OKGraph2Sparql(Graph2Sparql):
    def make_graph_pattern(self, query_graph: QueryGraph, s: str, o: str):
        s_sparql = self._resolve_node_to_sparql(query_graph, s)
        p = query_graph.edges[s, o]["label"]

        if p in [
            "ocape:hasReactant",
            "ocape:hasProduct",
            "okin:belongsToPhase/okin:containedIn",
        ] and query_graph.nodes[o].get("template_node"):
            p_sparql = p + "/rdfs:label"
            o_sparql = '"{label}"'.format(label=query_graph.nodes[o]["label"])
        else:
            p_sparql = p
            o_sparql = self._resolve_node_to_sparql(query_graph, o)

        return "{s} {p} {o} .".format(s=s_sparql, p=p_sparql, o=o_sparql)
