import random

import numpy as np

from constants.ontozeolite import (
    CRYSTAL_ATTR_LABELS,
    ZEOMATERIAL_ATTR_LABELS,
    OZCrystalInfoAttrKey,
    OZMaterialAttrKey,
)
from utils.numerical import normalize_1d
from locate_then_ask.graph2sparql import Graph2Sparql
from locate_then_ask.query_graph import QueryGraph


class OZMaterialAsker:
    ASK_ATTR_WEIGHT = {
        OZMaterialAttrKey.FRAMEWORK: 1,
        OZMaterialAttrKey.CRYSTAL_INFO: 4,
        OZMaterialAttrKey.GUEST_COMPOUND: 1,
    }

    def __init__(self):
        self.graph2sparql = Graph2Sparql()

    def _get_unsampled_keys(self, query_graph: QueryGraph):
        sampled_keys = set(
            [key for _, _, key in query_graph.out_edges("Material", data="key")]
        )
        return tuple(
            [key for key in tuple(OZMaterialAttrKey) if key not in sampled_keys]
        )

    def ask_name(self, query_graph: QueryGraph, verbalization: str):
        query_graph.add_question_node("Material")
        query_sparql = self.graph2sparql.convert(query_graph)
        return query_sparql, "what are the {located}?".format(located=verbalization)

    def ask_attr(self, query_graph: QueryGraph, verbalization: str):
        unsampled_keys = self._get_unsampled_keys(query_graph)
        key = np.random.choice(
            unsampled_keys,
            p=normalize_1d([self.ASK_ATTR_WEIGHT[x] for x in unsampled_keys]),
        )
        if key is OZMaterialAttrKey.FRAMEWORK:
            query_graph.add_question_node("Framework")
            query_graph.add_triple("Material", "^zeo:hasZeoliticMaterial", "Framework")
            qnode_verbn = "framework"
        elif key is OZMaterialAttrKey.GUEST_COMPOUND:
            query_graph.add_question_node("GuestCompoundFormula")
            query_graph.add_triple(
                "Material", "zeo:hasGuestCompound/os:formula", "GuestCompoundFormula"
            )
            qnode_verbn = random.choice(ZEOMATERIAL_ATTR_LABELS[key])
        elif key is OZMaterialAttrKey.CRYSTAL_INFO:
            k = random.choice(tuple(OZCrystalInfoAttrKey))
            if k is OZCrystalInfoAttrKey.COORD_TRANSFORM:
                target_coord = random.choice(["Cartesian", "Fractional"])
                transform_matrix_node = "TransformationMatrixTo" + target_coord
                transform_vector_node = "TransformationVectorTo" + target_coord
                query_graph.add_question_node(transform_matrix_node)
                query_graph.add_question_node(transform_vector_node)

                bn = query_graph.make_blank_node()
                query_graph.add_triples(
                    [
                        (
                            "Material",
                            "ocr:hasCrystalInformation/ocr:hasCoordinateTransformation",
                            bn,
                        ),
                        (bn, "ocr:has" + transform_matrix_node, transform_matrix_node),
                        (bn, "ocr:has" + transform_vector_node, transform_vector_node),
                    ]
                )
                if target_coord == "Cartesian":
                    src, target = "fractional", "Cartesian"
                else:
                    src, target = "Cartesian", "fractional"
                qnode_verbn = (
                    "transformation from {src} to {target} coordinate system".format(
                        src=src, target=target
                    )
                )
            else:
                query_graph.add_question_node(k.value)
                qnode_verbn = random.choice(CRYSTAL_ATTR_LABELS[k])
                query_graph.add_triple(
                    "Material", "ocr:hasCrystalInformation/ocr:has" + k.value, k.value
                )
        else:
            raise ValueError("Unexpected key: " + key)
        query_sparql = self.graph2sparql.convert(query_graph)

        template = random.choice(
            ["For {located}, what is the {qn}?", "What is the {qn} of {located}?"]
        )
        verbn = template.format(located=verbalization, qn=qnode_verbn)

        return query_sparql, verbn
