import random

import numpy as np

from constants.ontozeolite import (
    CRYSTAL_ATTR_LABELS,
    ZEOTOPO_ATTR_LABELS,
    OZCrystalInfoAttrKey,
    OZFrameworkAttrKey,
    OZZeoTopoAttrKey,
)
from utils.numerical import normalize_1d
from locate_then_ask.graph2sparql import Graph2Sparql
from locate_then_ask.query_graph import QueryGraph


FRAMEWORK_ATTRS_FOR_ASKING = {
    OZFrameworkAttrKey.FRAMEWORK_COMPONENTS: 2,
    OZFrameworkAttrKey.CRYSTAL_INFO: 2,
    OZFrameworkAttrKey.TOPO_ATTR: 10,
    OZFrameworkAttrKey.GUEST_SPECIES: 2,
}

class OZFrameworkAsker:
    def __init__(self):
        self.graph2sparql = Graph2Sparql()

    def _get_unsampled_keys(self, query_graph: QueryGraph):
        sampled_keys = set(
            [key for _, _, key in query_graph.edges.data("key")]
        )
        return tuple(
            [
                key
                for key in FRAMEWORK_ATTRS_FOR_ASKING
                if key not in sampled_keys
            ]
        )

    def ask_name(self, query_graph: QueryGraph, verbalization: str):
        query_graph.add_question_node("Framework")
        query_sparql = self.graph2sparql.convert(query_graph)
        return query_sparql, "what are the {located}?".format(located=verbalization)

    def ask_attr(self, query_graph: QueryGraph, verbalization: str):
        unsampled_keys = self._get_unsampled_keys(query_graph)
        key = np.random.choice(unsampled_keys, p=normalize_1d([FRAMEWORK_ATTRS_FOR_ASKING[k] for k in unsampled_keys]))

        if key is OZFrameworkAttrKey.FRAMEWORK_COMPONENTS:
            query_graph.add_triples([
                "Framework", "zeo:hasZeoliticMaterial", "Material",
                "Material", "zeo:hasFrameworkComponent/rdfs:label", "FrameworkComponent"
            ])
            query_graph.add_question_node("FrameworkComponent")
            qnode_verbn = random.choice(["building elements", "framework components"])
        elif key is OZFrameworkAttrKey.CRYSTAL_INFO:
            crystal_key = random.choice(tuple(OZCrystalInfoAttrKey))
            if crystal_key is OZCrystalInfoAttrKey.COORD_TRANSFORM:
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
                qnode = crystal_key.value
                query_graph.add_question_node(qnode)
                pred = "ocr:hasCrystalInformation/ocr:has" + crystal_key.value
                qnode_verbn = random.choice(CRYSTAL_ATTR_LABELS[crystal_key])
                query_graph.add_triple("Framework", pred, qnode)
        elif key is OZFrameworkAttrKey.TOPO_ATTR:
            topo_key = random.choice(tuple(OZZeoTopoAttrKey))
            qnode = topo_key.value
            query_graph.add_question_node(qnode)
            pred = "zeo:hasZeoliticProperties/zeo:has" + topo_key.value
            qnode_verbn = random.choice(ZEOTOPO_ATTR_LABELS[topo_key])
            query_graph.add_triple("Framework", pred, qnode)
        elif key is OZFrameworkAttrKey.GUEST_SPECIES:
            query_graph.add_triples([
                "Framework", "zeo:hasZeoliticMaterial", "Material",
                "Material", "zeo:hasGuestCompound/rdfs:label", "GuestCompound"
            ])
            query_graph.add_question_node("GuestCompound")
            qnode_verbn = random.choice(["guest", "guest compound", "guest species", "incorporated species"])

        query_sparql = self.graph2sparql.convert(query_graph)

        template = random.choice(
            ["For {located}, what is the {qn}?", "What is the {qn} of {located}?"]
        )
        verbn = template.format(located=verbalization, qn=qnode_verbn)

        return query_sparql, verbn
