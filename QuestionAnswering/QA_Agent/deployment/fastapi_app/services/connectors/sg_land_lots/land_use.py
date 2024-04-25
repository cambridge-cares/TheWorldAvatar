from collections import defaultdict
from functools import cache, lru_cache
from typing import Annotated, DefaultDict, List, Set

from fastapi import Depends

from services.kg import get_sgPlot_bgClient
from services.core.kg import KgClient
from services.connectors.sg_land_lots.model import LandUseTypeNode


class LandUseTypeStore:
    def __init__(self, nodes: List[LandUseTypeNode]):
        self.nodes = nodes

        clsname2iris: DefaultDict[str, Set[str]] = defaultdict(set)
        iri2nodes: DefaultDict[str, List[LandUseTypeNode]] = defaultdict(list)
        for node in nodes:
            clsname2iris[node.clsname].add(node.IRI)
            iri2nodes[node.IRI].append(node)

        self.clsname2iris = clsname2iris
        self.iri2nodes = iri2nodes

    def get_all(self):
        return self.nodes

    # def get_iris(self, clsname: str):
    #     return list(self.clsname2iris[clsname])

    @lru_cache(maxsize=64)
    def get_clsnames(self, iri: str):
        return list(set([node.clsname for node in self.iri2nodes[iri]]))

    @lru_cache(maxsize=64)
    def get_labels(self, iri: str):
        return list(set([node.label for node in self.iri2nodes[iri]]))


def get_landUseType_nodes(kg_client: KgClient):
    # Currently there are no triples ontozoning:Agriculture rdfs:subClass ontozoning:LandUseType
    # Thus, the detection of LandUseType classes relies on the IRI suffix of LandUseType instances

    query = """SELECT DISTINCT ?IRI ?clsname ?label ?comment WHERE {
BIND (REPLACE(STR(?IRI), "^.*/([^/]*)$", "$1") as ?InstanceName)
FILTER (strstarts(?InstanceName, "LandUseType"))

?IRI rdf:type ?LandUseTypeClass .
BIND (REPLACE(STR(?LandUseTypeClass), "^.*/([^/]*)$", "$1") as ?clsname)

?IRI rdfs:label ?label . 
?IRI rdfs:comment ?comment . 
}"""
    res = kg_client.query(query)
    bindings = [
        {k: v["value"] for k, v in binding.items()}
        for binding in res["results"]["bindings"]
    ]

    return [LandUseTypeNode(**binding) for binding in bindings]


@cache
def get_landUseType_store(bg_client: Annotated[KgClient, Depends(get_sgPlot_bgClient)]):
    nodes = get_landUseType_nodes(bg_client)
    return LandUseTypeStore(nodes)
