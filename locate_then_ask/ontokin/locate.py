import copy
import random
from locate_then_ask.ontokin.entity_store import OKEntityStore
from locate_then_ask.ontokin.model import (
    OCAPEProduct,
    OCAPEReactant,
    OKGasePhaseReaction,
    OKMechanism,
    OKSpecies,
)
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
        return query_graph, "mechanism"

    def locate_concept_and_relation(self, query_graph: QueryGraph):
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
            predicate="^okin:containedIn/^okin:belongsToPhase",
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

        species_node = "Species_" + str(len(sampled_species_nodes))
        query_graph.add_node(
            species_node,
            iri=species_iri,
            rdf_type="okin:Species",
            label=species.label,
            template_node=True,
        )
        query_graph.add_edge(
            "Mechanism",
            species_node,
            label="^okin:containedIn/^okin:belongsToPhase",
        )

        verbalization = "contains [{label}]".format(label=species.label)

        return query_graph, verbalization

    def locate_intersection(self, entity_iri: str, cond_num: int):
        pass


class OKReactionLocator:
    def __init__(self, store: OKEntityStore):
        self.store = store

    def locate_entity_name(self, entity_iri: str):
        entity = self.store.get(entity_iri)
        assert isinstance(entity, OKGasePhaseReaction)

        label = entity.equation

        query_graph = QueryGraph()
        query_graph.add_node(
            "Reaction",
            iri=entity_iri,
            rdf_type="okin:GasPhaseReaction",
            label=label,
            template_node=True,
            topic_entity=True,
        )

        verbalization = "[{entity}]".format(entity=label)

        return query_graph, verbalization

    def locate_concept_name(self, entity_iri: str):
        query_graph = QueryGraph()
        query_graph.add_node(
            "Reaction",
            iri=entity_iri,
            rdf_type="okin:GasPhaseReaction",
            label="okin:GasPhaseReaction",
            topic_entity=True,
        )
        return query_graph, "reaction"

    def locate_concept_and_relation(self, query_graph: QueryGraph):
        query_graph = copy.deepcopy(query_graph)
        topic_node = next(
            n
            for n, topic_entity in query_graph.nodes(data="topic_entity")
            if topic_entity
        )
        entity_iri = query_graph.nodes[topic_node]["iri"]
        entity = self.store.get(entity_iri)
        assert isinstance(entity, OKGasePhaseReaction)

        sampled_reactant_nodes = get_objs(
            query_graph, subj="Reaction", predicate="ocape:hasReactant"
        )
        sampled_reactant_iris = [
            query_graph.nodes[n]["iri"] for n in sampled_reactant_nodes
        ]
        unsampled_reactants = [
            x for x in entity.reactants if x.iri not in sampled_reactant_iris
        ]

        sampled_product_nodes = get_objs(
            query_graph, subj="Mechanism", predicate="ocape:hasProducts"
        )
        sampled_product_iris = [
            query_graph.nodes[n]["iri"] for n in sampled_product_nodes
        ]
        unsampled_products = [
            x for x in entity.products if x.iri not in sampled_product_iris
        ]

        sampling_frame = unsampled_reactants + unsampled_products
        sampled = random.choice(sampling_frame)
        if isinstance(sampled, OCAPEReactant):
            reactant_node = "Reactant_" + str(len(sampled_reactant_nodes))
            query_graph.add_node(
                reactant_node,
                iri=sampled.iri,
                rdf_type="ocape:Reactant",
                label=sampled.label,
                template_node=True,
            )
            query_graph.add_edge("Reaction", reactant_node, label="ocape:hasReactant")
            verbalization = "has reactant [{label}]".format(label=sampled.label)
        elif isinstance(sampled, OCAPEProduct):
            product_node = "Product_" + str(len(sampled_product_nodes))
            query_graph.add_node(
                product_node,
                iri=sampled.iri,
                rdf_type="ocape:Product",
                label=sampled.label,
                template_node=True,
            )
            query_graph.add_edge("Mechanism", product_node, label="ocape:hasProduct")
            verbalization = "has product [{label}]".format(label=sampled.label)
        else:
            raise ValueError("Unexpected type: " + type(sampled))

        return query_graph, verbalization

    def locate_intersection(self, entity_iri: str, cond_num: int):
        pass


class OKSpeciesLocator:
    def __init__(self, store: OKEntityStore):
        self.store = store

    def locate_entity_name(self, entity_iri: str):
        entity = self.store.get(entity_iri)
        assert isinstance(entity, OKSpecies)

        label = entity.label

        query_graph = QueryGraph()
        query_graph.add_node(
            "Species",
            iri=entity_iri,
            rdf_type="okin:Species",
            label=label,
            template_node=True,
            topic_entity=True,
        )

        verbalization = "[{entity}]".format(entity=label)

        return query_graph, verbalization

    def locate_concept_name(self, entity_iri: str):
        query_graph = QueryGraph()
        query_graph.add_node(
            "Species",
            iri=entity_iri,
            rdf_type=r"okin:Species",
            label="okin:Species",
            topic_entity=True,
        )
        return query_graph, "chemical species"

    def locate_concept_and_relation(self, entity_iri: str):
        query_graph, verbalization = self.locate_concept_name(entity_iri)
        entity = self.store.get(entity_iri)
        assert isinstance(entity, OKSpecies)

        query_graph.add_node(
            "Mechanism",
            iri=entity.mechanism.iri,
            rdf_type="okin:Mechanism",
            label=entity.mechanism.label,
            template_node=True,
        )
        query_graph.add_edge(
            "Species", "Mechanism", "okin:belongsToPhase/ontokin:containedIn"
        )

        verbalization + " in the reaction mechanism [{label}]".format(
            label=entity.mechanism.label
        )

        return query_graph, verbalization
