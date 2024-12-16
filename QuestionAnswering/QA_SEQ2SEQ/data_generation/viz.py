import networkx as nx

from utils.ontology import UtilsOntology



def plotGraph(G: nx.MultiDiGraph, filepath: str = "graph.svg"):
    G = nx.relabel_nodes(G, mapping={n: UtilsOntology.shorten_iri(n) for n in G.nodes()})
    labels = {
        e: UtilsOntology.shorten_iri(prop) for e, prop in nx.get_edge_attributes(G, "label").items()
    }
    nx.set_edge_attributes(G, labels, "label")

    for n, attr in G.nodes(data=True):
        G.nodes[n]["shape"] = "box"

        if attr.get("question_node"):
            G.nodes[n]["penwidth"] = 3

        if attr.get("template_node"):
            G.nodes[n]["style"] = "filled"
            G.nodes[n]["fillcolor"] = "#CCCCCC"

        if attr.get("literal") or n == "rdfs:Literal":
            G.nodes[n]["shape"] = "diamond"
        if attr.get("func"):
            G.nodes[n]["shape"] = "circle"

    G.graph["edge"] = {"arrowsize": "0.6", "splines": "curved"}
    G.graph["graph"] = {"scale": "3"}

    A = nx.drawing.nx_agraph.to_agraph(G)
    A.layout("dot")
    A.draw(filepath)
