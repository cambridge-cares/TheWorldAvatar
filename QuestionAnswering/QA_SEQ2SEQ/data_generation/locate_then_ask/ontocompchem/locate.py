import random
from typing import List
from constants.functions import StrOp

from locate_then_ask.ontocompchem.entity_store import OCCEntityStore
from locate_then_ask.ontocompchem.model import OCCMolecularComputation, OCCSpecies
from locate_then_ask.query_graph import QueryGraph


class OCCLocator:
    def __init__(self):
        self.store = OCCEntityStore()

    def locate(self, entity_iri: str):
        species = self.store.get(entity_iri)
        assert isinstance(species, OCCSpecies)

        query_graph = QueryGraph()
        query_graph.add_topic_node("MolecularComputation")
        species_label_node = query_graph.make_literal_node(value=species.label)
        query_graph.add_triple("MolecularComputation", "occ:hasSpeciesModel/occ:hasSpecies/rdfs:label", species_label_node)

        verbalization = "[{label}]".format(label=species.label)

        molcomps: List[OCCMolecularComputation] = [
            self.store.get(iri) for iri in species.molecular_computation_iris
        ]

        # locate level of theory
        if random.getrandbits(1):
            lots = set([x.level_of_theory for x in molcomps])
            num = random.sample(population=[1, 2], counts=[3, 1], k=1)[0]
            lots_sampled = random.sample(lots, k=min(num, len(lots)))

            query_graph.add_literal_node("LevelOfTheoryLabel")
            query_graph.add_func("LevelOfTheoryLabel", operator=StrOp.VALUES, operand=lots_sampled)
            query_graph.add_triple("MolecularComputation", "occ:hasMethodology/occ:hasLevelOfTheory/rdfs:label", "LevelOfTheoryLabel")

            verbn_template = random.choice(
                ["at the {LOT} level", "at the {LOT} level of theory"]
            )
            verbn_lot = verbn_template.format(
                LOT=" or ".join(["[{label}]".format(label=x) for x in lots_sampled])
            )
        else:
            verbn_lot = None

        # locate basis set
        if random.getrandbits(1):
            bss = set([x.basis_set for x in molcomps])
            num = random.sample(population=[1, 2, 3], counts=[9, 3, 1], k=1)[0]
            bss_sampled = random.sample(bss, k=min(num, len(bss)))

            query_graph.add_literal_node("BasisSetLabel")
            query_graph.add_func("BasisSetLabel", operator=StrOp.VALUES, operand=bss_sampled)
            query_graph.add_triple("MolecularComputation", "occ:hasMethodology/occ:hasBasisSet/rdfs:label", "BasisSetLabel")

            verbn_template = "using the {BS} basis set"
            verbn_bs = verbn_template.format(
                BS=" or ".join(["[{label}]".format(label=x) for x in bss_sampled])
            )
        else:
            verbn_bs = None

        if verbn_lot is not None or verbn_bs is not None:
            verbalization += " calculated"
        if verbn_lot is not None:
            verbalization += " " + verbn_lot
        if verbn_bs is not None:
            verbalization += " " + verbn_bs

        return query_graph, verbalization
