import networkx as nx

from utils import Utils


def plotGraph(G: nx.MultiDiGraph):
    G = nx.relabel_nodes(G, mapping={n: Utils.shortenIri(n) for n in G.nodes()})
    labels = {
        e: Utils.shortenIri(prop) for e, prop in nx.get_edge_attributes(G, "label").items()
    }
    nx.set_edge_attributes(G, labels, "label")

    for n, attr in G.nodes(data=True):
        if attr.get("question_node"):
            G.nodes[n]["penwidth"] = 3
        if attr.get("template_node"):
            G.nodes[n]["style"] = "filled"
            G.nodes[n]["fillcolor"] = "#CCCCCC"

    G.graph["edge"] = {"arrowsize": "0.6", "splines": "curved"}
    G.graph["graph"] = {"scale": "3"}

    A = nx.drawing.nx_agraph.to_agraph(G)
    A.layout("dot")
    A.draw("graph.svg")
