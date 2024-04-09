import random

from locate_then_ask.ontokin.entity_store import OKEntityStore
from locate_then_ask.ontokin.make_example_base import ExampleMakerBase
from .locate import OKSpeciesLocator
from .ask import OKSpeciesAsker


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
            ask_strategy = "attribute_or_relation"
        elif locate_strategy == "concept_and_relation":
            (
                query_graph,
                verbalization,
            ) = self.locator.locate_concept_and_relation(entity_iri)
            ask_strategy = random.sample(["name", "count", "attribute_or_relation"], counts=[1, 1, 4], k=1)[0]
        else:
            raise Exception()

        if ask_strategy == "name":
            query_sparql, verbalization = self.asker.ask_name(query_graph, verbalization)
        elif ask_strategy == "count":
            query_sparql, verbalization = self.asker.ask_count(query_graph, verbalization)
        elif ask_strategy == "attribute_or_relation":
            attr_num = random.sample(population=[1, 2], counts=[2, 1], k=1)[0]
            query_sparql, verbalization = self.asker.ask_attribute_or_relation(
                query_graph, verbalization, attr_num=attr_num
            )
        else:
            raise Exception()

        return query_graph, query_sparql, verbalization