import random
from typing import List

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
        query_graph.add_nodes_from(
            [
                (
                    "MolecularComputation",
                    dict(
                        rdf_type="occ:MolecularComputation",
                        label="occ:MolecularComputation",
                        topic_entity=True
                    )
                ), (
                    "Species",
                    dict(
                        iri=species.iri,
                        rdf_type="os:Species",
                        label=species.label,
                        template_node=True
                    )
                )
        ])
        query_graph.add_edge("MolecularComputation", "Species", label="occ:hasSpeciesModel/occ:hasSpecies")
        
        verbalization = "[{label}]".format(label=species.label)

        molcomps: List[OCCMolecularComputation] = [self.store.get(iri) for iri in species.molecular_computation_iris]
        
        # locate level of theory
        if random.getrandbits(1):
            lots = set([x.level_of_theory for x in molcomps])
            num = random.sample(population=[1, 2], counts=[3, 1], k=1)[0]
            lots_sampled = random.sample(lots, k=min(num, len(lots)))
            
            literal_num = len(
                [n for n in query_graph.nodes() if n.startswith("Literal")]
            )
            node = "Literal_" + str(literal_num)
            query_graph.add_node(node, label=lots_sampled, template_node=True, literal=True)
            query_graph.add_edge("MolecularComputation", node, label="occ:hasMethodology/occ:hasLevelOfTheory/rdfs:label")

            verbn_template = random.choice(["at the {LOT} level", "at the {LOT} level of theory"])
            verbn_lot = verbn_template.format(LOT=" or ".join(["[{label}]".format(label=x) for x in lots_sampled]))
        else:
            verbn_lot = None

        # locate basis set
        if random.getrandbits(1):
            bss = set([x.basis_set for x in molcomps])
            num = random.sample(population=[1, 2, 3], counts=[9, 3, 1], k=1)[0]
            bss_sampled = random.sample(bss, k=min(num, len(bss)))
            
            literal_num = len(
                [n for n in query_graph.nodes() if n.startswith("Literal")]
            )
            node = "Literal_" + str(literal_num)
            query_graph.add_node(node, label=bss_sampled, template_node=True, literal=True)
            query_graph.add_edge("MolecularComputation", node, label="occ:hasMethodology/occ:hasBasisSet/rdfs:label")

            verbn_template = "using the {label} basis set"
            verbn_bs = verbn_template.format(label=" or ".join(bss_sampled))
        else:
            verbn_bs = None

        if verbn_lot is not None and verbn_bs is not None:
            verbalization += " calculated"
        if verbn_lot is not None:
            verbalization += " " + verbn_lot
        if verbn_bs is not None:
            verbalization += " " + verbn_bs

        return query_graph, verbalization