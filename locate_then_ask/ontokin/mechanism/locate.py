import copy
import random

from locate_then_ask.ontokin.entity_store import OKEntityStore
from locate_then_ask.ontokin.model import OKMechanism
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

    def _locate_concept_and_relation(self, query_graph: QueryGraph):
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

        verbalization = "invovles [{label}]".format(label=species.label)

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