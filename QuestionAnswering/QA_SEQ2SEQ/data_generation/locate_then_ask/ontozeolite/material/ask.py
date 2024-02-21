import random

from constants.ontozeolite import (
    CRYSTAL_ATTR_LABELS,
    ZEOMATERIAL_ATTR_LABELS,
    OZCrystalInfoAttrKey,
    OZMaterialAttrKey,
)
from locate_then_ask.graph2sparql import Graph2Sparql
from locate_then_ask.query_graph import QueryGraph


class OZMaterialAsker:
    def __init__(self):
        self.graph2sparql = Graph2Sparql()

    def _get_unsampled_keys(self, query_graph: QueryGraph):
        sampled_keys = set(
            [key for _, _, key in query_graph.out_edges("Material", data="key")]
        )
        return tuple(
            [
                key
                for key in tuple(OZMaterialAttrKey)
                if key not in sampled_keys
            ]
        )

    def ask_name(self, query_graph: QueryGraph, verbalization: str):
        query_graph.add_question_node("Material")
        query_sparql = self.graph2sparql.convert(query_graph)
        return query_sparql, "what are the {located}?".format(located=verbalization)

    def ask_attr(self, query_graph: QueryGraph, verbalization: str):
        key = random.choice(self._get_unsampled_keys(query_graph))
        if key is OZMaterialAttrKey.FRAMEWORK:
            query_graph.add_question_node("Framework")
            query_graph.add_triple("Material", "^zeo:hasZeoliticMaterial", "Framework")
            qnode_verbn = "framework"
        elif key is OZMaterialAttrKey.CRYSTAL_INFO:
            k = random.choice(tuple(OZCrystalInfoAttrKey))
            query_graph.add_question_node(k.value)
            qnode_verbn = random.choice(CRYSTAL_ATTR_LABELS[k])
            query_graph.add_triple("Material", "ocr:hasCrystsalInformation/ocr:has" + k.value, k.value)
        elif key is OZMaterialAttrKey.GUEST_COMPOUND:
            query_graph.add_question_node("GuestCompoundFormula")
            query_graph.add_triple("Material", "zeo:hasGuestCompound/os:formula", "GuestCompoundFormula")
            qnode_verbn = random.choice(ZEOMATERIAL_ATTR_LABELS[key])
        query_sparql = self.graph2sparql.convert(query_graph)

        template = random.choice(
            ["For {located}, what is the {qn}?", "What is the {qn} of {located}?"]
        )
        verbn = template.format(located=verbalization, qn=qnode_verbn)

        return query_sparql, verbn
