from collections import defaultdict
from typing import Iterable

from locate_then_ask.query_graph import QueryGraph


def count_schema_items(query_graphs: Iterable[QueryGraph]):
    prop2freq = defaultdict(lambda: 0)
    cls2freq = defaultdict(lambda: 0)

    for query_graph in query_graphs:
        for s, o, p in query_graph.edges(data="label"):
            props = p.split("/")
            if props[-1] in ["a", "rdfs:subClassOf", "rdfs:subClassOf*"] and query_graph.nodes[o].get("template_node"):
                cls2freq[o] += 1
            if props[0] in ["rdfs:subClassOf", "rdfs:subClassOf*"] and query_graph.nodes[s].get("template_node"):
                cls2freq[s] += 1

            stack = []
            for prop in props:
                if prop.startswith("(") and prop.endswith(")") and "|" in prop:
                    stack.extend(prop[1:-1].split("|"))
                else:
                    stack.append(prop)

            while stack:
                prop = stack.pop()
                if prop.startswith("^"):
                    prop = prop[1:]
                prop2freq[prop] += 1

    return {"property": prop2freq, "class": cls2freq}
