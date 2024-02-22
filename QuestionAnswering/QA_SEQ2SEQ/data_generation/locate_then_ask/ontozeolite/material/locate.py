import random

from constants.ontozeolite import (
    ZEOMATERIAL_ATTR_LABELS,
    OZMaterialAttrKey,
)
from locate_then_ask.query_graph import QueryGraph
from locate_then_ask.ontozeolite.entity_store import OZEntityStore


class OZMaterialLocator:
    ATTR_KEYS = [
        OZMaterialAttrKey.FRAMEWORK,
        OZMaterialAttrKey.GUEST_COMPOUND,
    ]

    def __init__(self, store: OZEntityStore):
        self.store = store

    def _locate_concept_name(self, entity_iri: str):
        query_graph = QueryGraph()
        query_graph.add_topic_node("Material", iri=entity_iri)
        return query_graph, random.choice(["zeolite", "zeolite material"])

    def locate_name(self, entity_iri):
        query_graph, concept = self._locate_concept_name(entity_iri)
        entity = self.store.get_material(entity_iri)
        formula = random.choice(entity.formulae)

        literal_node = query_graph.make_literal_node(formula)
        query_graph.add_triple("Material", "zeo:hasChemicalFormula", literal_node)
        verbn = "{concept} [{label}]".format(
            concept=concept + random.choice(["", " formulation"]), label=formula
        )

        return query_graph, verbn

    def locate_concept_and_literal_multi(self, entity_iri: str, cond_num: int):
        query_graph, concept = self._locate_concept_name(entity_iri)
        entity = self.store.get_material(entity_iri)

        conds = []
        for k in random.sample(self.ATTR_KEYS, k=min(len(self.ATTR_KEYS), cond_num)):
            if k is OZMaterialAttrKey.FRAMEWORK:
                framework = self.store.get_framework(entity.framework_iri)
                literal_node = query_graph.make_literal_node(framework.framework_code)
                query_graph.add_triple(
                    "?Material", "^zeo:hasZeoliticMaterial", literal_node, key=k
                )
                cond = "whose framework is " + framework.framework_code
            elif k is OZMaterialAttrKey.GUEST_COMPOUND:
                literal_node = query_graph.make_literal_node(entity.guest_compound)
                query_graph.add_triple(
                    "?Material", "zeo:hasGuestCompound/os:formula", literal_node, key=k
                )
                cond = "whose {attr} is {val}".format(
                    attr=random.choice(ZEOMATERIAL_ATTR_LABELS[k])
                )
            else:
                raise Exception("Unexpected k: " + str(k))
            conds.append(cond)

        return query_graph, "{concept}, {attrs}".format(
            concept=concept, attrs=", and ".join(conds)
        )
