from locate_then_ask.ontokin.entity_store import OKEntityStore
from locate_then_ask.ontokin.model import OKSpecies
from locate_then_ask.query_graph import QueryGraph


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
        return query_graph, "the chemical species"

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
            "Species", "Mechanism", label="okin:belongsToPhase/okin:containedIn"
        )

        verbalization += " that appears in the reaction mechanism [{label}]".format(
            label=entity.mechanism.label
        )

        return query_graph, verbalization
