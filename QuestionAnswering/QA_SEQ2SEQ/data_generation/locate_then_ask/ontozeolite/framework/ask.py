import random

from constants.ontozeolite import (
    CRYSTAL_ATTR_LABELS,
    ZEOTOPO_ATTR_LABELS,
    OZCrystalInfoAttrKey,
    OZZeoTopoAttrKey,
)
from locate_then_ask.graph2sparql import Graph2Sparql
from locate_then_ask.query_graph import QueryGraph


class OZFrameworkAsker:
    def __init__(self):
        self.graph2sparql = Graph2Sparql()

    def _get_unsampled_keys(self, query_graph: QueryGraph):
        sampled_keys = set(
            [key for _, _, key in query_graph.out_edges("Framework", data="key")]
        )
        return tuple(
            [
                key
                for key in tuple(OZCrystalInfoAttrKey) + tuple(OZZeoTopoAttrKey)
                if key not in sampled_keys
            ]
        )

    def ask_name(self, query_graph: QueryGraph, verbalization: str):
        query_graph.add_question_node("Framework")
        query_sparql = self.graph2sparql.convert(query_graph)
        return query_sparql, "what are the {located}?".format(located=verbalization)

    def ask_attr(self, query_graph: QueryGraph, verbalization: str):
        key = random.choice(self._get_unsampled_keys(query_graph))
        if key is OZCrystalInfoAttrKey.COORD_TRANSFORM:
            target_coord = random.choice(["Cartesian", "Fractional"])
            transform_matrix_node = "TransformationMatrixTo" + target_coord
            transform_vector_node = "TransformationVectorTo" + target_coord
            query_graph.add_question_node(transform_matrix_node)
            query_graph.add_question_node(transform_vector_node)

            bn = query_graph.make_blank_node()
            query_graph.add_triples(
                [
                    (
                        "Framework",
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
            qnode = key.value
            query_graph.add_question_node(qnode)
            if isinstance(key, OZCrystalInfoAttrKey):
                pred = "ocr:hasCrystalInformation/ocr:has" + key.value
                qnode_verbn = random.choice(CRYSTAL_ATTR_LABELS[key])
            else:
                pred = "zeo:hasZeoliteTopology/zeo:has" + key.value
                qnode_verbn = random.choice(ZEOTOPO_ATTR_LABELS[key])
            query_graph.add_triple("Framework", pred, qnode)

        query_sparql = self.graph2sparql.convert(query_graph)

        template = random.choice(
            ["For {located}, what is the {qn}?", "What is the {qn} of {located}?"]
        )
        verbn = template.format(located=verbalization, qn=qnode_verbn)

        return query_sparql, verbn
