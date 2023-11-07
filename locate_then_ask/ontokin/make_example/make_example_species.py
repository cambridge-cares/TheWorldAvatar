import random

from locate_then_ask.ontokin.ask.ask_species import OKSpeciesAsker
from locate_then_ask.ontokin.entity_store import OKEntityStore
from locate_then_ask.ontokin.locate.locate_species import OKSpeciesLocator
from locate_then_ask.ontokin.make_example.make_example_base import ExampleMakerBase


class OKSpeciesExampleMaker(ExampleMakerBase):
    def __init__(self, store: OKEntityStore):
        self.locator = OKSpeciesLocator(store)
        self.asker = OKSpeciesAsker()

    def make_example(self, entity_iri: str):
        locate_strategy = random.choice(["entity_name", "concept_and_relation"])
        if locate_strategy == "entity_name":
            (
                query_graph,
                verbalization,
            ) = self.locator.locate_entity_name(entity_iri)
            ask_strategies = ["attribute_or_relation"]
        elif locate_strategy == "concept_and_relation":
            (
                query_graph,
                verbalization,
            ) = self.locator.locate_concept_and_relation(entity_iri)
            ask_strategies = ["name", "attribute_or_relation"]
        else:
            raise Exception()

        ask_strategy = random.choice(ask_strategies)
        if ask_strategy == "name":
            ask_datum = self.asker.ask_name(query_graph, verbalization)
        elif ask_strategy == "attribute_or_relation":
            ask_datum = self.asker.ask_attribute_or_relation(
                query_graph, verbalization
            )
        else:
            raise Exception()
        
        return ask_datum