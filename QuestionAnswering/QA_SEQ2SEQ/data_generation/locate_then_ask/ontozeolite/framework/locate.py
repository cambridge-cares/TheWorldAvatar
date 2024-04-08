from collections import defaultdict
from decimal import Decimal
import random
from typing import DefaultDict, Dict, List

import numpy as np

from constants.functions import BASIC_NUM_OPS
from constants.ontozeolite import (
    CRYSTAL_SCALAR_KEYS,
    ZEOTOPO_ATTR_LABELS,
    OZCrystalInfoAttrKey,
    OZFrameworkAttrKey,
    OZZeoTopoAttrKey,
)
from locate_then_ask.ontozeolite.model import OZCrystalInfo
from utils.numerical import make_operand_and_verbn, normalize_1d
from locate_then_ask.query_graph import QueryGraph
from locate_then_ask.ontozeolite.entity_store import OZEntityStore


class OZFrameworkLocator:
    def __init__(self, store: OZEntityStore):
        self.store = store

    def locate_concept_name(self, entity_iri: str):
        query_graph = QueryGraph()
        query_graph.add_topic_node("Framework", iri=entity_iri)
        return query_graph, np.random.choice(["zeolite framework", "zeolite"], p=normalize_1d([4, 1]))

    def locate_name(self, entity_iri):
        query_graph, concept = self.locate_concept_name(entity_iri)
        entity = self.store.get_framework(entity_iri)

        literal_node = query_graph.make_literal_node(entity.framework_code)
        query_graph.add_triple("Framework", "zeo:hasFrameworkCode", literal_node)
        verbn = "{concept} [{label}]".format(
            concept=concept, label=entity.framework_code
        )

        return query_graph, verbn

    def _locate_framework_components(
        self, query_graph: QueryGraph, framework_components: List[str], freq: int
    ):
        elements = random.sample(
            framework_components, k=min(len(framework_components), freq)
        )
        literal_nodes = [query_graph.make_literal_node(elem) for elem in elements]
        only = random.getrandbits(1)
        pred = "zeo:hasFrameworkComponent{only}/rdfs:label".format(
            only="Only" if only else ""
        )

        query_graph.add_triple("Framework", "zeo:hasZeoliticMaterial", "Material")
        query_graph.add_triples(
            [
                (
                    "Material",
                    pred,
                    literal_node,
                    dict(key=OZFrameworkAttrKey.FRAMEWORK_COMPONENTS),
                )
                for literal_node in literal_nodes
            ]
        )

        return "which {contain} {elements}".format(
            contain=random.choice(["contain", "are built by"]) + " only" if only else "",
            elements=" and ".join(
                "[{literal}]".format(literal=literal) for literal in elements
            ),
        )

    def _locate_crystal_info(
        self, query_graph: QueryGraph, crystal_info: OZCrystalInfo, freq: int
    ):
        frame = [OZCrystalInfoAttrKey.UNIT_CELL]
        if crystal_info.tile_code:
            frame.append(OZCrystalInfoAttrKey.TILED_STRUCTURE)
            
        keys = random.sample(frame, k=freq)
        conds = []
        for k in keys:
            if k is OZCrystalInfoAttrKey.UNIT_CELL:
                val_node = "UnitCellVolumeNumericalValue"

                query_graph.add_literal_node(val_node)
                query_graph.add_triple(
                    "Framework",
                    "ocr:hasCrystalInformation/ocr:hasUnitCell/ocr:hasUnitCellVolume/om:hasNumericalValue",
                    val_node,
                    key=OZFrameworkAttrKey.CRYSTAL_INFO,
                    subkey=k,
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
                    key=OZFrameworkAttrKey.CRYSTAL_INFO,
                    subkey=k,
                )

                cond = "whose tile code is [{label}]".format(
                    label=crystal_info.tile_code
                )
            else:
                raise ValueError("Unexpected crystal info key: " + str(k))
            
            conds.append(cond)

        return conds

    def _locate_topo_attr(
        self, query_graph: QueryGraph, topo_scalar: Dict[OZZeoTopoAttrKey, Decimal], freq: int
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
            query_graph.add_triple(
                "Framework", pred, val_node, key=OZFrameworkAttrKey.TOPO_ATTR, subkey=k
            )

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

    def _locate_material(self, query_graph: QueryGraph, material_iris: List[str]):
        material_iri = random.choice(material_iris)
        material = self.store.get_material(material_iri)

        literal_node = query_graph.make_literal_node(material.formula)
        query_graph.add_triples(
            [
                ("Framework", "zeo:hasZeoliticMaterial", "Material"),
                (
                    "Material",
                    "zeo:hasChemicalFormula",
                    literal_node,
                    dict(key=OZFrameworkAttrKey.MATERIALS),
                ),
            ]
        )

        return "corresponding to {material} {formula}".format(
            material=random.choice(["material", "zeolitic material", "zeolite"]),
            formula=random.choice(["formula ", "with formula ", ""]) + material.formula,
        )

    def _locate_guest_species(
        self, query_graph: QueryGraph, guest_species_iris: List[str], freq: int
    ):
        guest_iris = random.sample(
            guest_species_iris, k=min(len(guest_species_iris), freq)
        )
        guests = [
            random.choice(self.store.get_guest_species_identifiers(iri))
            for iri in guest_iris
        ]
        literal_nodes = [query_graph.make_literal_node(guest) for guest in guests]

        query_graph.add_triple("Framework", "zeo:hasZeoliticMaterial", "Material")
        query_graph.add_triples(
            [
                (
                    "Material",
                    "zeo:hasGuestCompound/rdfs:label",
                    literal,
                    dict(key=OZFrameworkAttrKey.GUEST_SPECIES),
                )
                for literal in literal_nodes
            ]
        )

        return "which {incorporate} {guests}".format(
            incorporate=random.choice(["incorporate", "have guest species"]),
            guests=" and ".join(guests),
        )

    def locate_concept_and_literal_multi(self, entity_iri: str, cond_num: int):
        query_graph, concept = self.locate_concept_name(entity_iri)

        entity = self.store.get_framework(entity_iri)

        attr_key_counts = {
            OZFrameworkAttrKey.FRAMEWORK_COMPONENTS: 6,
            OZFrameworkAttrKey.CRYSTAL_INFO: 2 if entity.crystal_info.tile_code else 1,
            OZFrameworkAttrKey.TOPO_ATTR: 10,
            OZFrameworkAttrKey.MATERIALS: 1,
            OZFrameworkAttrKey.GUEST_SPECIES: 10,
        }
        attr2freq: DefaultDict[OZFrameworkAttrKey, int] = defaultdict(lambda: 0)
        for k in random.sample(
            attr_key_counts.keys(),
            k=min(cond_num, sum(attr_key_counts.values())),
            counts=attr_key_counts.values(),
        ):
            attr2freq[k] += 1

        conds = []
        for attr, freq in attr2freq.items():
            if attr is OZFrameworkAttrKey.FRAMEWORK_COMPONENTS:
                _cond = self._locate_framework_components(
                    query_graph, entity.framework_components, freq
                )
                conds.append(_cond)
            elif attr is OZFrameworkAttrKey.CRYSTAL_INFO:
                _conds = self._locate_crystal_info(
                    query_graph, entity.crystal_info, freq
                )
                conds.extend(_conds)
            elif attr is OZFrameworkAttrKey.TOPO_ATTR:
                _conds = self._locate_topo_attr(query_graph, entity.topo_scalar, freq)
                conds.extend(_conds)
            elif attr is OZFrameworkAttrKey.MATERIALS:
                _cond = self._locate_material(query_graph, entity.material_iris)
                conds.append(_cond)
            elif attr is OZFrameworkAttrKey.GUEST_SPECIES:
                _cond = self._locate_guest_species(
                    query_graph, entity.guest_species_iris, freq
                )
                conds.append(_cond)
            else:
                raise Exception("Unexpected attr: " + str(attr))

        return query_graph, "{concept}, {attrs}".format(
            concept=concept, attrs=", and ".join(conds)
        )
