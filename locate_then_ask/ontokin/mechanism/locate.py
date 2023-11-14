import copy
import random

from locate_then_ask.ontokin.entity_store import OKEntityStore
from locate_then_ask.ontokin.model import OKGasPhaseReaction, OKMechanism, OKSpecies
from locate_then_ask.query_graph import QueryGraph, get_objs


class OKMechanismLocator:
    def __init__(self, store: OKEntityStore):
        self.store = store

    def locate_entity_name(self, entity_iri: str):
        label = self.store.get(entity_iri).label

        query_graph = QueryGraph()
        query_graph.add_node(
            "Mechanism",
            iri=entity_iri,
            rdf_type="okin:ReactionMechanism",
            label=label,
            template_node=True,
            topic_entity=True,
        )

        verbalization = "[{entity}]".format(entity=label)

        return query_graph, verbalization

    def locate_concept_name(self, entity_iri: str):
        query_graph = QueryGraph()
        query_graph.add_node(
            "Mechanism",
            iri=entity_iri,
            rdf_type="okin:ReactionMechanism",
            label="okin:ReactionMechanism",
            topic_entity=True,
        )
        return query_graph, "reaction mechanism"

    def _locate_concept_and_relation_species(self, query_graph: QueryGraph):
        query_graph = copy.deepcopy(query_graph)
        topic_node = next(
            n
            for n, topic_entity in query_graph.nodes(data="topic_entity")
            if topic_entity
        )
        entity_iri = query_graph.nodes[topic_node]["iri"]
        entity = self.store.get(entity_iri)
        assert isinstance(entity, OKMechanism)

        sampled_species_nodes = get_objs(
            query_graph,
            subj="Mechanism",
            predicate="okin:hasGasPhase/^okin:belongsToPhase",
        )
        sampled_species_iris = [
            query_graph.nodes[n]["iri"] for n in sampled_species_nodes
        ]
        unsampled_species_iris = [
            x for x in entity.species_iris if x not in sampled_species_iris
        ]

        if len(unsampled_species_iris) == 0:
            return query_graph, ""

        species_iri = random.choice(unsampled_species_iris)
        species = self.store.get(species_iri)
        assert isinstance(species, OKSpecies)
        
        species_node = "Species_" + str(len(sampled_species_nodes))
        query_graph.add_node(
            species_node,
            iri=species_iri,
            rdf_type="os:Species",
            label=species.label,
            template_node=True,
        )
        query_graph.add_edge(
            "Mechanism",
            species_node,
            label="okin:hasGasPhase/^okin:belongsToPhase",
        )

        verbalization = "invovles [{label}]".format(label=species.label)

        return query_graph, verbalization

    def _locate_concept_and_relation_reaction(self, query_graph: QueryGraph):
        query_graph = copy.deepcopy(query_graph)
        topic_node = next(
            n
            for n, topic_entity in query_graph.nodes(data="topic_entity")
            if topic_entity
        )
        entity_iri = query_graph.nodes[topic_node]["iri"]
        entity = self.store.get(entity_iri)
        assert isinstance(entity, OKMechanism)

        sampled_rxn_nodes = get_objs(
            query_graph,
            subj="Mechanism",
            predicate="okin:hasReaction",
        )
        sampled_rxn_iris = [
            query_graph.nodes[n]["iri"] for n in sampled_rxn_nodes
        ]
        unsampled_rxn_iris = [
            x for x in entity.reaction_iris if x not in sampled_rxn_iris
        ]

        if len(unsampled_rxn_iris) == 0:
            return query_graph, ""

        rxn_iri = random.choice(unsampled_rxn_iris)
        rxn = self.store.get(rxn_iri)
        assert isinstance(rxn, OKGasPhaseReaction)

        rxn_node = "Reaction_" + str(len(sampled_rxn_nodes))
        rxn_eqn = random.choice(rxn.equations)
        query_graph.add_node(
            rxn_node,
            iri=rxn_iri,
            rdf_type="okin:GasPhaseReaction",
            label=rxn_eqn,
            template_node=True,
        )
        query_graph.add_edge(
            "Mechanism",
            rxn_node,
            label="okin:hasReaction",
        )

        verbalization = "invovles the reaction [{label}]".format(label=rxn_eqn)

        return query_graph, verbalization
    
    def locate_concept_and_relation_multi(self, entity_iri: str, cond_num: int, obj_type: str):
        verbalized_conds = []
        query_graph, concept = self.locate_concept_name(entity_iri)

        if obj_type == "species":
            locate_func = self._locate_concept_and_relation_species
        elif obj_type == "reaction":
            locate_func = self._locate_concept_and_relation_reaction
        else:
            raise ValueError("Expected obj_type to be either `species` or `reaction`, found: " + obj_type)
        
        for _ in range(cond_num):
            query_graph, verbalized_cond = locate_func(query_graph)
            if verbalized_cond:
                verbalized_conds.append(verbalized_cond)

        verbalization = "the {concept} that {conds}".format(
            concept=concept, conds=" and ".join(verbalized_conds)
        )

        return query_graph, verbalization
