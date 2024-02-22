import random

from locate_then_ask.ontokin.entity_store import OKEntityStore
from locate_then_ask.ontokin.model import OKSpecies
from locate_then_ask.query_graph import QueryGraph


class OKSpeciesLocator:
    def __init__(self, store: OKEntityStore):
        self.store = store

    def locate_entity_name(self, entity_iri: str):
        entity = self.store.get_species(entity_iri)
        label = entity.label

        query_graph = QueryGraph()
        query_graph.add_topic_node("Species", iri=entity_iri)
        literal_node = query_graph.make_literal_node(value=entity.label)
        query_graph.add_triple("Species", "skos:altLabel", literal_node)

        verbalization = "[{entity}]".format(entity=label)

        return query_graph, verbalization

    def locate_concept_and_relation(self, entity_iri: str):
        query_graph = QueryGraph()
        query_graph.add_topic_node("Species", iri=entity_iri)

        entity = self.store.get_species(entity_iri)
        mechanism_iri = random.choice(entity.mechanism_iris)
        mechanism = self.store.get_mechanism(mechanism_iri)

        query_graph.add_node("Mechanism", iri=mechanism.iri)
        doi_node = query_graph.make_literal_node(value=mechanism.doi)
        query_graph.add_triples(
            [
                ("Species", "okin:belongsToPhase/^okin:hasGasPhase", "Mechanism"),
                ("Mechanism", "okin:hasProvenance/op:hasDOI", doi_node),
            ]
        )
        
        verbalization = "the chemical species that appears in the reaction mechanism {verb} in [{DOI}]".format(
            verb=random.choice(
                    ["found", "proposed", "outlined", "indicated", "shown"]
                ),
            DOI=mechanism.doi
        )

        return query_graph, verbalization
