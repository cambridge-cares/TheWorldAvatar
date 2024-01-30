import networkx as nx

QueryGraph = nx.DiGraph


def get_preds(query_graph: QueryGraph, subj: str):
    return [p for u, _, p in query_graph.edges(data="label") if u == subj]


def get_objs(query_graph: QueryGraph, subj: str, predicate: str):
    return [
        v
        for u, v, label in query_graph.edges(data="label")
        if u == subj and label == predicate
    ]
