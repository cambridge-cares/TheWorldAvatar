import random

from locate_then_ask.ontokin.make_example_base import ExampleMakerBase
from locate_then_ask.ontokin.entity_store import OKEntityStore
from .locate import OKMechanismLocator
from .ask import OKMechanismAsker


class OKMechanismExampleMaker(ExampleMakerBase):
    def __init__(self, store: OKEntityStore):
        self.locator = OKMechanismLocator(store)
        self.asker = OKMechanismAsker()

    def make_example(self, entity_iri: str):
        cond_num = random.choice([1, 2, 3])
        query_graph, verbalization = self.locator.locate_concept_and_relation_multi(entity_iri, cond_num=cond_num)
        return self.asker.ask_name(query_graph, verbalization)
