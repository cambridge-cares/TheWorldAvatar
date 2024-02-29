from abc import ABC, abstractmethod

import networkx as nx
from typing import List, Tuple

class Regrounder(ABC):
    @abstractmethod
    def reground(self, query_graph: nx.DiGraph, query_sparql: str, paraphrases: List[str]) -> List[Tuple[str, str]]:
        pass

class IdentityRegrounder(Regrounder):
    def reground(self, query_graph: nx.DiGraph, query_sparql: str, paraphrases: List[str]):
        return [(query_sparql, p) for p in paraphrases]