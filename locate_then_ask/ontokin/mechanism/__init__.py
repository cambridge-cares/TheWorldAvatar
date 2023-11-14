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
        obj_type = random.choice(["species", "reaction"])

        if obj_type == "species":
            cond_num = random.choice([1, 2, 3])
        elif obj_type == "reaction":
            cond_num = random.sample(population=[1, 2, 3], counts=[4, 2, 1], k=1)[0]

        query_graph, verbalization = self.locator.locate_concept_and_relation_multi(entity_iri, cond_num=cond_num, obj_type=obj_type)
        return self.asker.ask_name(query_graph, verbalization)
