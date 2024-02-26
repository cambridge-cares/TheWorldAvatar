from collections import defaultdict
from typing import Iterable, List

from locate_then_ask.query_graph import QueryGraph


def _is_obj_a_type(query_graph: QueryGraph, s: str, prop_path: Iterable[str], o: str):
    return prop_path[-1] in [
        "a",
        "rdfs:subClassOf",
        "rdfs:subClassOf*",
    ] and query_graph.nodes[o].get("template_node")


def _is_subj_a_type(query_graph: QueryGraph, s: str, prop_path: Iterable[str], o: str):
    return prop_path[0] in [
        "rdfs:subClassOf",
        "rdfs:subClassOf*",
    ] and query_graph.nodes[s].get("template_node")


def _decompose_proppath(prop_path: Iterable[str]):
    stack: List[str] = []
    for prop in prop_path:
        if prop.startswith("(") and prop.endswith(")") and "|" in prop:
            stack.extend(prop[1:-1].split("|"))
        else:
            stack.append(prop)
    return stack


def count_schema_items(query_graphs: Iterable[QueryGraph]):
    prop2freq = defaultdict(lambda: 0)
    cls2freq = defaultdict(lambda: 0)

    for query_graph in query_graphs:
        for s, o, p in query_graph.edges(data="label"):
            props = p.split("/")
            if _is_obj_a_type(query_graph, s, props, o):
                cls2freq[o] += 1
            if _is_subj_a_type(query_graph, s, p, o):
                cls2freq[s] += 1

            for prop in _decompose_proppath(props):
                if prop.startswith("^"):
                    prop = prop[1:]
                prop2freq[prop] += 1

    prop2freq = {k: prop2freq[k] for k in sorted(prop2freq.keys())}
    cls2freq = {k: cls2freq[k] for k in sorted(cls2freq.keys())}

    return {"property": prop2freq, "class": cls2freq}
