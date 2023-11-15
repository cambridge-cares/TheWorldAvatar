import copy
import random

from locate_then_ask.ontokin.entity_store import OKEntityStore
from locate_then_ask.ontokin.model import (
    OKGasPhaseReaction,
    OKMechanism,
    OKSpecies,
)
from locate_then_ask.query_graph import QueryGraph, get_objs


class OKReactionLocator:
    def __init__(self, store: OKEntityStore):
        self.store = store

    def locate_concept_and_attribute(self, entity_iri: str):
        entity = self.store.get(entity_iri)
        assert isinstance(entity, OKGasPhaseReaction)

        eqn = random.choice(entity.equations)

        query_graph = QueryGraph()
        literal_node = "Literal_0"
        query_graph.add_nodes_from(
            [
                (
                    "Reaction",
                    dict(
                        iri=entity_iri,
                        rdf_type="okin:GasPhaseReaction",
                        topic_entity=True,
                    ),
                ),
                (literal_node, dict(label=eqn, literal=True, template_node=True)),
            ]
        )
        query_graph.add_edge("Reaction", literal_node, label="okin:hasEquation")

        verbalization = "the chemical reaction [{entity}]".format(entity=eqn)

        if "Mechanism" not in query_graph.nodes() and random.getrandbits(1):
            mechanism_iri = random.choice(entity.mechanism_iris)
            mechanism = self.store.get(mechanism_iri)
            assert isinstance(mechanism, OKMechanism)

            mechanism_node = "Mechanism"
            literal_node = "Literal_1"
            query_graph.add_nodes_from(
                [
                    (
                        mechanism_node,
                        dict(iri=mechanism.iri, rdf_type="okin:ReactionMechanism"),
                    ),
                    (
                        literal_node,
                        dict(label=mechanism.doi, literal=True, template_node=True),
                    ),
                ]
            )
            query_graph.add_edges_from(
                [
                    ("Reaction", mechanism_node, dict(label="^okin:hasReaction")),
                    (
                        mechanism_node,
                        literal_node,
                        dict(label="okin:hasProvenance/oprvn:hasDOI"),
                    ),
                ]
            )
            verbalization += " involved in the mechanism found in [{label}]".format(
                label=mechanism.doi
            )

        return query_graph, verbalization

    def _locate_concept_name(self, entity_iri: str):
        query_graph = QueryGraph()
        query_graph.add_node(
            "Reaction",
            iri=entity_iri,
            rdf_type="okin:GasPhaseReaction",
            label="okin:GasPhaseReaction",
            topic_entity=True,
        )
        return query_graph, "chemical reaction"

    def _locate_concept_and_relation(self, query_graph: QueryGraph):
        query_graph = copy.deepcopy(query_graph)
        topic_node = next(
            n
            for n, topic_entity in query_graph.nodes(data="topic_entity")
            if topic_entity
        )
        entity_iri = query_graph.nodes[topic_node]["iri"]
        entity = self.store.get(entity_iri)
        assert isinstance(entity, OKGasPhaseReaction)

        sampled_reactant_nodes = get_objs(
            query_graph, subj="Reaction", predicate="ocape:hasReactant"
        )
        sampled_reactant_iris = [
            query_graph.nodes[n]["iri"] for n in sampled_reactant_nodes
        ]
        unsampled_reactant_iris = [
            x for x in entity.reactant_iris if x not in sampled_reactant_iris
        ]

        sampled_product_nodes = get_objs(
            query_graph, subj="Reaction", predicate="ocape:hasProduct"
        )
        sampled_product_iris = [
            query_graph.nodes[n]["iri"] for n in sampled_product_nodes
        ]
        unsampled_product_iris = [
            x for x in entity.product_iris if x not in sampled_product_iris
        ]

        sampling_frame = unsampled_reactant_iris + unsampled_product_iris

        sampled_mechanism_nodes = get_objs(
            query_graph,
            subj="Reaction",
            predicate="^okin:hasReaction",
        )
        assert len(sampled_mechanism_nodes) <= 1
        if len(sampled_mechanism_nodes) == 0:
            sampling_frame += [random.choice(entity.mechanism_iris)]

        if len(sampling_frame) == 0:
            return query_graph, ""

        sampled_iri = random.choice(sampling_frame)
        sampled_entity = self.store.get(sampled_iri)
        if sampled_iri in unsampled_reactant_iris:
            assert isinstance(sampled_entity, OKSpecies)
            reactant_node = "Reactant_" + str(len(sampled_reactant_nodes))
            query_graph.add_node(
                reactant_node,
                iri=sampled_entity.iri,
                rdf_type="os:Species",
                label=sampled_entity.label,
                template_node=True,
            )
            query_graph.add_edge("Reaction", reactant_node, label="ocape:hasReactant")
            verbalization = "has reactant [{label}]".format(label=sampled_entity.label)
        elif sampled_iri in unsampled_product_iris:
            assert isinstance(sampled_entity, OKSpecies)
            product_node = "Product_" + str(len(sampled_product_nodes))
            query_graph.add_node(
                product_node,
                iri=sampled_entity.iri,
                rdf_type="os:Species",
                label=sampled_entity.label,
                template_node=True,
            )
            query_graph.add_edge("Reaction", product_node, label="ocape:hasProduct")
            verbalization = "has product [{label}]".format(label=sampled_entity.label)
        else:
            assert isinstance(sampled_entity, OKMechanism)
            mechanism_node = "Mechanism"
            literal_node = "Literal_" + str(sum(n.startswith("Literal_") for n in query_graph.nodes()))
            query_graph.add_nodes_from(
                [
                    (
                        mechanism_node,
                        dict(iri=sampled_entity.iri, rdf_type="okin:ReactionMechanism"),
                    ),
                    (
                        literal_node,
                        dict(
                            label=sampled_entity.doi, literal=True, template_node=True
                        ),
                    ),
                ]
            )
            query_graph.add_edges_from(
                [
                    ("Reaction", mechanism_node, dict(label="^okin:hasReaction")),
                    (
                        mechanism_node,
                        literal_node,
                        dict(label="okin:hasProvenance/oprvn:hasDOI"),
                    ),
                ]
            )
            verbalization = "is involved in the mechanism found in [{DOI}]".format(
                DOI=sampled_entity.doi
            )

        return query_graph, verbalization

    def locate_concept_and_relation_multi(self, entity_iri: str, cond_num: int):
        verbalized_conds = []
        query_graph, concept = self._locate_concept_name(entity_iri)

        for _ in range(cond_num):
            query_graph, verbalized_cond = self._locate_concept_and_relation(
                query_graph
            )
            if verbalized_cond:
                verbalized_conds.append(verbalized_cond)

        verbalization = "the {concept} that {conds}".format(
            concept=concept, conds=" and ".join(verbalized_conds)
        )

        return query_graph, verbalization
