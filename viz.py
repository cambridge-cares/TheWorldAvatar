import networkx as nx

from constants import NAMESPACE2PREFIX


def shortenIri(iri: str):
    for ns, p in NAMESPACE2PREFIX.items():
        if iri.startswith(ns):
            return f"{p}:{iri[len(ns):]}"
    raise iri


def plotGraph(G: nx.MultiDiGraph):
    G = nx.relabel_nodes(G, mapping={n: shortenIri(n) for n in G.nodes()})
    labels = {
        e: shortenIri(prop) for e, prop in nx.get_edge_attributes(G, "label").items()
    }
    nx.set_edge_attributes(G, labels, "label")

    try:
        question_node = next(n for n in G.nodes(data="question_node") if n[1])[0]
        G.nodes[question_node]["color"] = "red"
    except:
        pass
    G.graph["edge"] = {"arrowsize": "0.6", "splines": "curved"}
    G.graph["graph"] = {"scale": "3"}

    A = nx.drawing.nx_agraph.to_agraph(G)
    A.layout("dot")
    A.draw("graph.svg")
