from typing import Iterable
import networkx as nx

from constants.namespaces import NAMESPACE2PREFIX
from constants.predicates import RDFS_SUBCLASSOF


class UtilsOntology:
    @classmethod
    def shorten_iri(cls, iri: str):
        for ns, p in NAMESPACE2PREFIX.items():
            if iri.startswith(ns):
                return f"{p}:{iri[len(ns):]}"
        return iri

    @classmethod
    def flatten_subclassof(cls, G: nx.DiGraph):
        def get_propedges(n: str):
            propedges = []
            for _, t, prop in G.out_edges(n, data="label"):
                if prop != RDFS_SUBCLASSOF:
                    propedges.append((n, t, dict(label=prop)))
                else:
                    propedges.extend([(n, tt, tprop) for _, tt, tprop in get_propedges(t)])
            return propedges
        add_edges = [e for n in G.nodes() for e in get_propedges(n)]
        remove_edges = [(h, t) for h, t, prop in G.edges(data="label") if prop == RDFS_SUBCLASSOF]
        G = G.copy()
        G.remove_edges_from(remove_edges)
        G.add_edges_from(add_edges)
        return G

    @classmethod
    def remove_egdes_by_label(cls, G: nx.DiGraph, labels: Iterable[str]):
        G = G.copy()
        remove_edges = [
            (h, t) for h, t, label in G.edges(data="label") if label in labels
        ]
        G.remove_edges_from(remove_edges)
        return G
    
    @classmethod
    def hash_kg_response(self, response):
        bindings = response["results"]["bindings"]

        if bindings is None:
            return tuple()

        # [
        #     Dict[
        #         str,
        #         {
        #             "datatype": NotRequired[str],
        #             "type": str
        #             "value" str
        #         }
        #     ]
        # ]
        _results = [[(k, v["value"]) for k, v in row.items()] for row in bindings]

        for row in _results:
            row.sort(key=lambda x: x[0])
        _results.sort(key=lambda row: tuple(x[1] for x in row))

        return tuple(tuple(row) for row in _results)