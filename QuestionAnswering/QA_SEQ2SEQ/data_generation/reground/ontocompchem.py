import json
import os
import random
from typing import List, Tuple

import networkx as nx
from constants.fs import ROOTDIR
from locate_then_ask.query_graph import QueryGraph
from reground.reground import Regrounder
from reground.utils import replace_nlq_literal, replace_sparql_literal


class OCCRegrounder(Regrounder):
    PATH_TO_ENTITIES_FOR_REGROUNDING = os.path.join(
        ROOTDIR, "data", "entities_for_regrounding", "ontocompchem.json"
    )

    def __init__(self):
        with open(self.PATH_TO_ENTITIES_FOR_REGROUNDING, "r") as f:
            self.entities_for_regrounding = json.load(f)

    def reground(self, query_graph: nx.DiGraph, query_sparql: str, paraphrases: List[str]):    
        query_graph = QueryGraph(query_graph)

        if "LevelOfTheoryLabel" in query_graph.nodes() and not query_graph.nodes["LevelOfTheoryLabel"].get("question_node"):
            func_node = query_graph.get_objs("LevelOfTheoryLabel", "func")[0]
            lots = query_graph.nodes[func_node]["operand"]
        else:
            lots = []

        if "BasisSetLabel" in query_graph.nodes() and not query_graph.nodes["BasisSetLabel"].get("question_node"):
            func_node = query_graph.get_objs("BasisSetLabel", "func")[0]
            bss = query_graph.nodes[func_node]["operand"]
        else:
            bss = []

        species = [] 
        for _, o, p in query_graph.edges(data="label"):
            if p == "occ:hasSpeciesModel/occ:hasSpecies/rdfs:label":
                species.append(query_graph.nodes[o]["label"])

        if not any([lots, bss, species]):
            return [(query_sparql, p) for p in paraphrases]

        pairs: List[Tuple[str, str]] = []
        for paraphrase in paraphrases:
            _query_sparql = str(query_sparql)
            for lot in lots:
                _lot = random.choice(self.entities_for_regrounding["LevelOfTheory"])
                paraphrase = replace_nlq_literal(paraphrase, old=lot, new=_lot)
                _query_sparql = replace_sparql_literal(_query_sparql, old=lot, new=_lot)
            for bs in bss:
                _bs = random.choice(self.entities_for_regrounding["BasisSet"])
                paraphrase = replace_nlq_literal(paraphrase, old=bs, new=_bs)
                _query_sparql = replace_sparql_literal(_query_sparql, old=bs, new=_bs)
            for s in species:
                _s = random.choice(self.entities_for_regrounding["Species"])
                paraphrase = replace_nlq_literal(paraphrase, old=s, new=_s)
                _query_sparql = replace_sparql_literal(_query_sparql, old=s, new=_s)

            pairs.append((_query_sparql, paraphrase))

        return pairs
