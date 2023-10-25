from collections import defaultdict
import itertools
import json
from typing import DefaultDict, Dict, List, Optional, Tuple

import rdflib
import networkx as nx

from constants.namespaces import OS
from constants.predicates import (
    OS_GITCOMMITHASH,
    OWL_CLASS,
    OWL_DATATYPEPROPERTY,
    OWL_EQUIVALENTCLASS,
    OWL_OBJECTPROPERTY,
    RDF_TYPE,
    RDFS_COMMENT,
    RDFS_DOMAIN,
    RDFS_ISDEFINEDBY,
    RDFS_RANGE,
    RDFS_SUBCLASSOF,
)


class Owl2LpgConverter:
    def __init__(self):
        self.reset()

    def reset(self):
        self.classes: List[str] = list()
        self.objprops: List[str] = list()
        self.dataprops: List[str] = list()

        self.prop2domains: DefaultDict[str, List[str]] = defaultdict(list)
        self.prop2ranges: DefaultDict[str, List[str]] = defaultdict(list)

        self.comments: Dict[str, str] = dict()
        self.subcls2supercls: List[Tuple[str, str]] = list()
        self.equivcls2equivcls: List[Tuple[str, str]] = list()

    def _do_skip_p(self, p):
        return p in [RDFS_ISDEFINEDBY, OS_GITCOMMITHASH]

    def _do_skip_so(self, sonode):
        return isinstance(sonode, rdflib.BNode) or str(sonode) == OS[:-1]

    def owl2lpg(
        self,
        owl_path: str,
        prop2domains_addon: Dict[str, List[str]],
        save_path: Optional[str] = None,
    ):
        self.reset()

        g = rdflib.Graph()
        g.parse(owl_path, format="application/rdf+xml")
        self.extract_rdftriples_data(g)

        G = nx.DiGraph()
        G.add_nodes_from([(n, attr) for n, attr in self.get_node2attr().items()])
        G.add_edges_from(
            [
                (u, v, attr)
                for (u, v), attr in self.get_edge2attr(
                    prop2domains_addon=prop2domains_addon
                ).items()
            ]
        )

        if save_path is not None:
            data = nx.node_link_data(G)
            with open(save_path, "w") as f:
                json.dump(data, f, indent=4)

        return G

    def extract_rdftriples_data(self, g: rdflib.Graph):
        for snode, pnode, onode in g:
            s, p, o = (str(x) for x in (snode, pnode, onode))
            if self._do_skip_p(p) or self._do_skip_so(snode) or self._do_skip_so(onode):
                pass
            elif p == RDF_TYPE:
                if not s.startswith(OS):
                    pass
                elif o == OWL_CLASS:
                    self.classes.append(s)
                elif o == OWL_OBJECTPROPERTY:
                    self.objprops.append(s)
                elif o == OWL_DATATYPEPROPERTY:
                    self.dataprops.append(s)
                else:
                    raise ValueError(f"Unexpected triple: \n{snode}\n{pnode}\n{onode}")
            elif p == RDFS_SUBCLASSOF:
                self.subcls2supercls.append((s, o))
            elif p == OWL_EQUIVALENTCLASS:
                if not s.startswith(OS) or not o.startswith(OS):
                    continue
                self.equivcls2equivcls.append((s, o))
            elif p == RDFS_DOMAIN:
                self.prop2domains[s].append(o)
            elif p == RDFS_RANGE:
                self.prop2ranges[s].append(o)
            elif p == RDFS_COMMENT:
                self.comments[s] = o
            else:
                raise ValueError(f"Unexpected triple: \n{snode}\n{pnode}\n{onode}")

    def get_node2attr(self):
        return {n: dict(comment=self.comments[n]) for n in self.classes}

    def get_edge2attr(self, prop2domains_addon: Dict[str, List[str]]):
        return {
            **self.get_edge_prop(),
            **self.get_edge_prop_addon(prop2domains_addon),
            **self.get_edge_subclassof(),
            **self.get_edge_equivalentclass(),
        }

    def get_edge_prop(self) -> Dict[Tuple[str, str], Dict[str, str]]:
        return {
            e: dict(label=prop, comment=self.comments.get(prop))
            for prop in self.objprops + self.dataprops
            for e in itertools.product(self.prop2domains[prop], self.prop2ranges[prop])
        }

    def get_edge_prop_addon(
        self, prop2domains_addon: Dict[str, List[str]]
    ) -> Dict[Tuple[str, str], Dict[str, str]]:
        return {
            e: dict(label=prop, comment=self.comments.get(prop))
            for prop, domains in prop2domains_addon.items()
            for e in itertools.product(domains, self.prop2ranges[prop])
        }

    def get_edge_subclassof(self):
        return {e: dict(label=RDFS_SUBCLASSOF) for e in self.subcls2supercls}

    def get_edge_equivalentclass(self):
        return {e: dict(label=OWL_EQUIVALENTCLASS) for e in self.equivcls2equivcls}
