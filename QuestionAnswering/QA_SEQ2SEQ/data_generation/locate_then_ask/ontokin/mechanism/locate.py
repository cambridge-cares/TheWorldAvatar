import random

from locate_then_ask.ontokin.entity_store import OKEntityStore
from locate_then_ask.ontokin.model import OKGasPhaseReaction, OKMechanism, OKSpecies
from locate_then_ask.query_graph import QueryGraph


class OKMechanismLocator:
    def __init__(self, store: OKEntityStore):
        self.store = store

    def _locate_concept_and_relation_species(
        self, query_graph: QueryGraph, cond_num: int = 1
    ):
        entity_iri = query_graph.nodes["Mechanism"]["iri"]
        entity = self.store.get_mechanism(entity_iri)

        species_iris = random.sample(
            entity.species_iris, k=min(cond_num, len(entity.species_iris))
        )
        labels = []
        for species_iri in species_iris:
            species = self.store.get_species(species_iri)

            labels.append(species.label)
            literal_node = query_graph.make_literal_node(value=species.label)
            query_graph.add_triple(
                "Mechanism",
                "okin:hasGasPhase/^okin:belongsToPhase/skos:altLabel",
                literal_node,
            )

        return "invovles {values}".format(
            values=" and ".join(["[{x}]".format(x=x) for x in labels])
        )

    def _locate_concept_and_relation_reaction(
        self, query_graph: QueryGraph, cond_num: int = 1
    ):
        entity_iri = query_graph.nodes["Mechanism"]["iri"]
        entity = self.store.get_mechanism(entity_iri)

        rxn_iris = random.sample(
            entity.reaction_iris, k=min(cond_num, len(entity.reaction_iris))
        )
        labels = []
        for rxn_iri in rxn_iris:
            rxn = self.store.get_rxn(rxn_iri)

            eqn = random.choice(rxn.equations)
            labels.append(eqn)
            literal_node = query_graph.make_literal_node(value=eqn)
            query_graph.add_triple(
                "Mechanism", "okin:hasReaction/okin:hasEquation", literal_node
            )

        return "invovles the reactions {values}".format(
            values=" and ".join(["[{x}]".format(x=x) for x in labels])
        )

    def locate_concept_and_relation_multi(
        self, entity_iri: str, cond_num: int, obj_type: str
    ):
        query_graph = QueryGraph()
        query_graph.add_topic_node("Mechanism", iri=entity_iri)

        if obj_type == "species":
            locate_func = self._locate_concept_and_relation_species
        elif obj_type == "reaction":
            locate_func = self._locate_concept_and_relation_reaction
        else:
            raise ValueError(
                "Expected obj_type to be either `species` or `reaction`, found: "
                + obj_type
            )

        verbalized_conds = locate_func(query_graph, cond_num)
        verbalization = "the reaction mechanism that " + verbalized_conds

        return query_graph, verbalization
