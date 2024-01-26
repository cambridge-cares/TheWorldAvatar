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
                        dict(label="okin:hasProvenance/op:hasDOI"),
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

    def _locate_by_species(self, query_graph: QueryGraph, entity: OKSpecies, type: str):
        if type == "reactant":
            predicate = "ocape:hasReactant"
            template = "has reactant [{label}]"
        elif type == "product":
            predicate = "ocape:hasProduct"
            template = "has product [{label}]"
        elif type == "either":
            predicate = "(ocape:hasReactant|ocape:hasProduct)"
            template = "{V} [{{label}}]".format(
                V=random.choice(["has", "features", "includes", "is participated by"])
            )
        else:
            raise ValueError("Unexpected species type wrt chemical reactions: " + type)

        node = "Species_" + str(
            sum(n.startswith("Species_") for n in query_graph.nodes())
        )

        query_graph.add_node(
            node,
            iri=entity.iri,
            rdf_type="os:Species",
            label=entity.label,
            template_node=True,
        )
        query_graph.add_edge("Reaction", node, label=predicate)
        verbalization = template.format(label=entity.label)
        return query_graph, verbalization

    def _locate_by_mechanism(
        self, query_graph: QueryGraph, sampled_entity: OKMechanism
    ):
        mechanism_node = "Mechanism"
        literal_node = "Literal_" + str(
            sum(n.startswith("Literal_") for n in query_graph.nodes())
        )
        query_graph.add_nodes_from(
            [
                (
                    mechanism_node,
                    dict(iri=sampled_entity.iri, rdf_type="okin:ReactionMechanism"),
                ),
                (
                    literal_node,
                    dict(label=sampled_entity.doi, literal=True, template_node=True),
                ),
            ]
        )
        query_graph.add_edges_from(
            [
                ("Reaction", mechanism_node, dict(label="^okin:hasReaction")),
                (
                    mechanism_node,
                    literal_node,
                    dict(label="okin:hasProvenance/op:hasDOI"),
                ),
            ]
        )
        verbalization = "is involved in the mechanism found in [{DOI}]".format(
            DOI=sampled_entity.doi
        )
        return query_graph, verbalization

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

        sampled_species_iris = [
            query_graph.nodes[n]["iri"]
            for n in query_graph.nodes()
            if n.startswith("Species_")
        ]
        unsampled_reactant_iris = [
            x for x in entity.reactant_iris if x not in sampled_species_iris
        ]
        unsampled_product_iris = [
            x for x in entity.product_iris if x not in sampled_species_iris
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
            query_graph, verbalization = self._locate_by_species(
                query_graph,
                entity=sampled_entity,
                type=random.choice(["reactant", "either"]),
            )
        elif sampled_iri in unsampled_product_iris:
            assert isinstance(sampled_entity, OKSpecies)
            query_graph, verbalization = self._locate_by_species(
                query_graph,
                entity=sampled_entity,
                type=random.choice(["product", "either"]),
            )
        else:
            assert isinstance(sampled_entity, OKMechanism)
            query_graph, verbalization = self._locate_by_mechanism(
                query_graph, sampled_entity
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
