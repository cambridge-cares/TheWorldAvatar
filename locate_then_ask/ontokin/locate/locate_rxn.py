import copy
import random

from locate_then_ask.ontokin.entity_store import OKEntityStore
from locate_then_ask.ontokin.model import OCAPEProduct, OCAPEReactant, OKGasePhaseReaction, OKMechanism
from locate_then_ask.query_graph import QueryGraph, get_objs


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

    def _locate_concept_and_relation(self, query_graph: QueryGraph):
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
            query_graph, subj="Reaction", predicate="ocape:hasProduct"
        )
        sampled_product_iris = [
            query_graph.nodes[n]["iri"] for n in sampled_product_nodes
        ]
        unsampled_products = [
            x for x in entity.products if x.iri not in sampled_product_iris
        ]

        sampled_mechanism_nodes = get_objs(
            query_graph, subj="Reaction", predicate="okin:belongsToPhase/okin:containedIn"
        )
        sampled_mechanism_iris = [
            query_graph.nodes[n]["iri"] for n in sampled_mechanism_nodes
        ]
        unsampled_mechanisms = [
            x for x in entity.mechanisms if x.iri not in sampled_mechanism_iris
        ]

        sampling_frame = unsampled_reactants + unsampled_products + unsampled_mechanisms
        if len(sampling_frame) == 0:
            return query_graph, ""
        
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
            query_graph.add_edge("Reaction", product_node, label="ocape:hasProduct")
            verbalization = "has product [{label}]".format(label=sampled.label)
        elif isinstance(sampled, OKMechanism):
            mechanism_node = "Mechanism_" + str(len(sampled_mechanism_nodes))
            query_graph.add_node(
                mechanism_node,
                iri=sampled.iri,
                rdf_type="okin:Mechanism",
                label=sampled.label,
                template_node=True,
            )
            query_graph.add_edge("Reaction", mechanism_node, label="okin:belongsToPhase/okin:containedIn")
            verbalization = "has mechanism [{label}]".format(label=sampled.label)
        else:
            raise ValueError("Unexpected type: " + type(sampled))

        return query_graph, verbalization

    def locate_concept_and_relation_multi(self, entity_iri: str, cond_num: int):
        verbalized_conds = []
        query_graph, concept = self.locate_concept_name(entity_iri)

        for _ in range(cond_num):
            query_graph, verbalized_cond = self._locate_concept_and_relation(query_graph)
            if verbalized_cond is not None:
                verbalized_conds.append(verbalized_cond)

        verbalization = "the {concept} that {conds}".format(
            concept=concept,
            conds=" and ".join(verbalized_conds)
        )

        return query_graph, verbalization