from collections import defaultdict
import random

from constants.functions import BASIC_NUM_OPS
from constants.ontozeolite import (
    ZEOTOPO_ATTR_LABELS,
    ZEOTOPO_SCALAR_KEYS,
    OZCrystalInfoAttrKey,
    OZFrameworkAttrKey,
    OZZeoTopoAttrKey,
)
from utils.numerical import make_operand_and_verbn
from locate_then_ask.query_graph import QueryGraph
from locate_then_ask.ontozeolite.entity_store import OZEntityStore


class OZFrameworkLocator:
    ATTR_KEY_WEIGHTS = {
        OZFrameworkAttrKey.CRYSTAL_INFO: 2,
        OZFrameworkAttrKey.TOPO_ATTR: 10,
    }

    def __init__(self, store: OZEntityStore):
        self.store = store

    def locate_concept_name(self, entity_iri: str):
        query_graph = QueryGraph()
        query_graph.add_topic_node("Framework", iri=entity_iri)
        return query_graph, "zeolite framework"

    def locate_name(self, entity_iri):
        query_graph, _ = self.locate_concept_name(entity_iri)
        entity = self.store.get_framework(entity_iri)

        literal_node = query_graph.make_literal_node(entity.framework_code)
        query_graph.add_triple("Framework", "zeo:hasFrameworkCode", literal_node)
        verbn = "zeolite framework [{label}]".format(label=entity.framework_code)

        return query_graph, verbn

    def locate_concept_and_literal_multi(self, entity_iri: str, cond_num: int):
        query_graph, concept = self.locate_concept_name(entity_iri)

        entity = self.store.get_framework(entity_iri)

        attr2freq: defaultdict[OZFrameworkAttrKey, int] = defaultdict(lambda: 0)
        for k in random.sample(
            tuple(self.ATTR_KEY_WEIGHTS.keys()),
            k=min(cond_num, len(self.ATTR_KEY_WEIGHTS)),
            counts=tuple(self.ATTR_KEY_WEIGHTS.values()),
        ):
            attr2freq[k] += 1

        crystalinfo_keys = random.sample(
            [OZCrystalInfoAttrKey.UNIT_CELL, OZCrystalInfoAttrKey.TILED_STRUCTURE],
            k=attr2freq[OZFrameworkAttrKey.CRYSTAL_INFO],
        )
        toposcalar_keys = random.sample(
            ZEOTOPO_SCALAR_KEYS, k=attr2freq[OZFrameworkAttrKey.TOPO_ATTR]
        )
        keys = crystalinfo_keys + toposcalar_keys
        random.shuffle(keys)

        conds = []
        for k in keys:
            if k is OZCrystalInfoAttrKey.UNIT_CELL or isinstance(k, OZZeoTopoAttrKey):
                if k is OZCrystalInfoAttrKey.UNIT_CELL:
                    attr_key = "UnitCellVolume"
                    pred = "ocr:hasCrystalInformation/ocr:hasUnitCell/ocr:hasUnitCellVolume/om:hasNumericalValue"
                    value = entity.crystal_info.unit_cell_volume
                    attr_verbn = "unit cell volume"
                else:
                    attr_key = k.value
                    pred = "zeo:hasZeoliteTopology/zeo:has{key}/om:hasNumericalValue".format(
                        key=attr_key
                    )
                    value = entity.topo_scalar[k]
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
            elif k is OZCrystalInfoAttrKey.TILED_STRUCTURE:
                literal_node = query_graph.make_literal_node(
                    entity.crystal_info.tile_code
                )
                query_graph.add_triple(
                    "Framework",
                    "ocr:hasCrystalInformation/ocr:hasTiledStructure/ocr:hasTile/ocr:hasTileCode",
                    literal_node,
                    key=k,
                )
                cond = "whose tile code is [{label}]".format(
                    label=entity.crystal_info.tile_code
                )
            else:
                raise Exception("Unexpected k: " + str(k))
            conds.append(cond)

        return query_graph, "{concept}, {attrs}".format(
            concept=concept, attrs=", and ".join(conds)
        )
