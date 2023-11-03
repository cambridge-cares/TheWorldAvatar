from typing import Iterable, Optional
from locate_then_ask.ontokin.entity_store import OKEntityStore
from locate_then_ask.ontokin.model import OKGasePhaseReaction, OKMechanism, OKSpecies
from locate_then_ask.query_graph import QueryGraph


class OKLocator:
    def __init__(self):
        self.store = OKEntityStore()

    def locate_entity_name(self, entity_iri: str):
        entity = self.store.get(entity_iri)

        if isinstance(entity, OKSpecies):
            class_name = "Species"
            entity_name = entity.label
        elif isinstance(entity, OKGasePhaseReaction):
            class_name = "Reaction"
            entity_name = entity.equation
        elif isinstance(entity, OKMechanism):
            class_name = "Mechanism"
            entity_name = entity.label

        query_graph = QueryGraph()
        query_graph.add_node(
            class_name, iri=entity_iri, label=entity_name, template_node=True
        )

        verbalization = "<entity>{entity}</entity>".format(entity=entity_name)

        return query_graph, verbalization

    def locate_concept_name(self, entity_iri: str):
        entity = self.store.get(entity_iri)

        if isinstance(entity, OKSpecies):
            class_name = "Species"
            rdf_type = "okin:Species"
            verbalization = "chemical species"
        elif isinstance(entity, OKGasePhaseReaction):
            class_name = "Reaction"
            rdf_type = "okin:GasPhaseReaction" # a/rdfs:subClassOf*
            verbalization = "reaction"
        elif isinstance(entity, OKMechanism):
            class_name = "Mechanism"
            rdf_type = "okin:ReactionMechanism"
            verbalization = "mechanism"

        query_graph = QueryGraph()
        query_graph.add_node(
            class_name, iri=entity_iri, rdf_type=rdf_type, label=rdf_type
        )
        return query_graph, verbalization

    def locate_concept_and_literal(
        self, entity_iri: str, query_graph: Optional[QueryGraph]
    ):
        pass

    def locate_intersection(self, entity_iri: str, cond_num: int):
        pass
