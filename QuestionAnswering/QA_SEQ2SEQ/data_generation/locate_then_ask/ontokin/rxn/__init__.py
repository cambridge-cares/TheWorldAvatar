import random

from locate_then_ask.ontokin.entity_store import OKEntityStore
from locate_then_ask.ontokin.make_example_base import ExampleMakerBase
from .locate import OKReactionLocator
from .ask import OKReactionAsker


class OKReactionExampleMaker(ExampleMakerBase):
    def __init__(self, store: OKEntityStore):
        self.locator = OKReactionLocator(store)
        self.asker = OKReactionAsker()

    def make_example(self, entity_iri: str):
        locate_strategy = random.sample(
            population=["concept_and_attribute", "concept_and_relation"],
            counts=[1, 3],
            k=1,
        )[0]
        if locate_strategy == "concept_and_attribute":
            query_graph, verbalization = self.locator.locate_concept_and_attribute(entity_iri)
            ask_strategies = ["relation"]
        elif locate_strategy == "concept_and_relation":
            cond_num = random.sample(population=[1, 2, 3, 4], counts=[4, 3, 2, 1], k=1)[
                0
            ]
            (
                query_graph,
                verbalization,
            ) = self.locator.locate_concept_and_relation_multi(
                entity_iri, cond_num=cond_num
            )
            if "Mechanism" in query_graph.nodes():
                ask_strategies = ["name", "count", "relation"]
            else:
                ask_strategies = ["name", "relation"]
        else:
            raise Exception()

        ask_strategy = random.choice(ask_strategies)
        if ask_strategy == "name":
            ask_datum = self.asker.ask_name(query_graph, verbalization)
        elif ask_strategy == "count":
            ask_datum = self.asker.ask_count(query_graph, verbalization)
        elif ask_strategy == "relation":
            ask_datum = self.asker.ask_relation(query_graph, verbalization)
        else:
            raise Exception()

        return ask_datum
