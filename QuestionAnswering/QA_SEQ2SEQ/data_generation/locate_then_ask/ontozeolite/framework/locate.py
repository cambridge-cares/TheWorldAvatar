from collections import defaultdict
from decimal import Decimal
import random
from typing import Dict

from constants.functions import BASIC_NUM_OPS
from constants.ontozeolite import (
    ZEOTOPO_ATTR_LABELS,
    OZCrystalInfoAttrKey,
    OZFrameworkAttrKey,
    OZZeoTopoAttrKey,
)
from locate_then_ask.ontozeolite.model import OZCrystalInfo
from utils.numerical import make_operand_and_verbn
from locate_then_ask.query_graph import QueryGraph
from locate_then_ask.ontozeolite.entity_store import OZEntityStore


class OZFrameworkLocator:
    def __init__(self, store: OZEntityStore):
        self.store = store

    def locate_concept_name(self, entity_iri: str):
        query_graph = QueryGraph()
        query_graph.add_topic_node("Framework", iri=entity_iri)
        return query_graph, random.choice(["zeolite framework", "zeolite"])

    def locate_name(self, entity_iri):
        query_graph, concept = self.locate_concept_name(entity_iri)
        entity = self.store.get_framework(entity_iri)

        literal_node = query_graph.make_literal_node(entity.framework_code)
        query_graph.add_triple("Framework", "zeo:hasFrameworkCode", literal_node)
        verbn = "{concept} [{label}]".format(
            concept=concept, label=entity.framework_code
        )

        return query_graph, verbn

    def _locate_crystal_info(
        self, query_graph: QueryGraph, crystal_info: OZCrystalInfo, freq: int
    ):
        keys = random.sample(tuple(OZCrystalInfoAttrKey), k=freq)
        conds = []
        for k in keys:
            if k is OZCrystalInfoAttrKey.UNIT_CELL:
                val_node = "UnitCellVolumeNumericalValue"

                query_graph.add_literal_node(val_node)
                query_graph.add_triple(
                    "Framework",
                    "ocr:hasCrystalInformation/ocr:hasUnitCell/ocr:hasUnitCellVolume/om:hasNumericalValue",
                    val_node,
                    key=k,
                )

                operator = random.choice(BASIC_NUM_OPS)
                operand, op_verbn = make_operand_and_verbn(
                    operator,
                    value=crystal_info.unit_cell_volume,
                    to_int=random.getrandbits(1),
                )
                query_graph.add_func(
                    target_node=val_node, operator=operator, operand=operand
                )

                cond = "whose unit cell volume is {op}".format(op=op_verbn)
            elif k is OZCrystalInfoAttrKey.TILED_STRUCTURE:
                assert crystal_info.tile_code is not None
                literal_node = query_graph.make_literal_node(crystal_info.tile_code)
                query_graph.add_triple(
                    "Framework",
                    "ocr:hasCrystalInformation/ocr:hasTiledStructure/ocr:hasTile/ocr:hasTileCode",
                    literal_node,
                    key=k,
                )
                cond = "whose tile code is [{label}]".format(
                    label=crystal_info.tile_code
                )
            conds.append(cond)

        return conds

    def _locate_topo_attr(
        query_graph: QueryGraph, topo_scalar: Dict[OZZeoTopoAttrKey, Decimal], freq: int
    ):
        conds = []

        for k, value in random.sample(topo_scalar.items(), k=freq):
            attr_key = k.value
            pred = "zeo:hasZeoliticProperties/zeo:has{key}/om:hasNumericalValue".format(
                key=attr_key
            )
            attr_verbn = random.choice(ZEOTOPO_ATTR_LABELS[k])
            val_node = attr_key + "NumericalValue"

            query_graph.add_literal_node(val_node)
            query_graph.add_triple("Framework", pred, val_node, key=k)

            operator = random.choice(BASIC_NUM_OPS)
            operand, op_verbn = make_operand_and_verbn(
                operator,
                value=value,
                to_int=random.getrandbits(1),
            )
            query_graph.add_func(
                target_node=val_node, operator=operator, operand=operand
            )

            cond = "whose {attr} is {op}".format(attr=attr_verbn, op=op_verbn)
            conds.append(cond)

        return conds

    def locate_concept_and_literal_multi(self, entity_iri: str, cond_num: int):
        query_graph, concept = self.locate_concept_name(entity_iri)

        entity = self.store.get_framework(entity_iri)

        attr_key_counts = {
            OZFrameworkAttrKey.FRAMEWORK_COMPONENTS: 2,
            OZFrameworkAttrKey.CRYSTAL_INFO: 2 if entity.crystal_info.tile_code else 1,
            OZFrameworkAttrKey.TOPO_ATTR: 10,
            OZFrameworkAttrKey.MATERIALS: 1,
            OZFrameworkAttrKey.GUEST_SPECIES: 5,
            OZFrameworkAttrKey.GUEST_FORMULA: 2,
        }
        attr2freq: defaultdict[OZFrameworkAttrKey, int] = defaultdict(lambda: 0)
        for k in random.sample(
            tuple(attr_key_counts.keys()),
            k=min(cond_num, len(attr_key_counts)),
            counts=tuple(attr_key_counts.values()),
        ):
            attr2freq[k] += 1

        conds = []
        for attr, freq in attr2freq.items():
            if attr is OZFrameworkAttrKey.FRAMEWORK_COMPONENTS:
                pass
            elif attr is OZFrameworkAttrKey.CRYSTAL_INFO:
                _conds = self._locate_crystal_info(
                    query_graph, entity.crystal_info, freq
                )
            elif attr is OZFrameworkAttrKey.TOPO_ATTR:
                _conds = self._locate_topo_attr(query_graph, entity.topo_scalar, freq)
            elif attr is OZFrameworkAttrKey.MATERIALS:
                pass
            elif attr is OZFrameworkAttrKey.GUEST_SPECIES:
                pass
            elif attr is OZFrameworkAttrKey.GUEST_FORMULA:
                pass
            else:
                raise Exception("Unexpected attr: " + str(attr))
            conds.extend(_conds)

        return query_graph, "{concept}, {attrs}".format(
            concept=concept, attrs=", and ".join(conds)
        )
