import numpy as np

from data_generation.utils.numerical import normalize_1d
from locate_then_ask.make_example_base import ExampleMakerBase
from locate_then_ask.ontozeolite.entity_store import OZEntityStore
from .locate import OZMaterialLocator
from .ask import OZMaterialAsker


class OZMaterialExampleMaker(ExampleMakerBase):
    def __init__(self, store: OZEntityStore):
        self.locator = OZMaterialLocator(store)
        self.asker = OZMaterialAsker(store)

    def make_example(self, entity_iri: str):
        locate_strategy = np.random.choice(["name", "attr"], p=normalize_1d([1, 2]))
        if locate_strategy == "name":
            query_graph, verbn = self.locator.locate_name(entity_iri)
            ask_strategies, ask_weights = ["attr"], [1]
        elif locate_strategy == "attr":
            cond_num = np.random.choice([1, 2], p=normalize_1d([2, 1]))
            query_graph, verbn = self.locator.locate_concept_and_literal_multi(
                entity_iri, cond_num=cond_num
            )
            ask_strategies, ask_weights = ["name", "attr"], [1, 5]
        else:
            raise Exception("Unexpected `locate_strategy`: " + locate_strategy)

        ask_strategy = np.random.choice(ask_strategies, p=normalize_1d([1, 4]))
        if ask_strategy == "name":
            query_sparql, verbn = self.asker.ask_name(query_graph, verbn)
        elif ask_strategy == "attr":
            query_sparql, verbn = self.asker.ask_attr(query_graph, verbn)
        else:
            raise Exception("Unexpected `ask_strategy`: " + ask_strategy)

        return query_graph, query_sparql, verbn
