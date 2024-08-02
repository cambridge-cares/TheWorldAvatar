import numpy as np

from locate_then_ask.make_example_base import ExampleMakerBase
from locate_then_ask.ontozeolite.entity_store import OZEntityStore
from utils.numerical import normalize_1d
from .ask import OZFrameworkAsker
from .locate import OZFrameworkLocator


class OZFrameworkExampleMaker(ExampleMakerBase):
    def __init__(self, store: OZEntityStore):
        self.locator = OZFrameworkLocator(store)
        self.asker = OZFrameworkAsker()

    def make_example(self, entity_iri: str):
        locate_strategy = np.random.choice(
            ["concept", "name", "attr"], p=normalize_1d([0.05, 1, 14])
        )
        if locate_strategy == "concept":
            query_graph, verbn = self.locator.locate_concept_name(entity_iri)
            ask_strategy_weights = {"name": 1, "attr": 1}
        elif locate_strategy == "name":
            query_graph, verbn = self.locator.locate_name(entity_iri)
            ask_strategy_weights = {"attr": 1}
        elif locate_strategy == "attr":
            cond_num = np.random.choice(
                [1, 2, 3, 4, 5], p=normalize_1d([3, 4, 5, 2, 1])
            )
            query_graph, verbn = self.locator.locate_concept_and_literal_multi(
                entity_iri, cond_num=cond_num
            )
            if self.asker.get_unsampled_keys(query_graph):
                ask_strategy_weights = {"name": 1, "attr": 10}
            else:
                ask_strategy_weights = {"name": 1}
        else:
            raise Exception("Unexpected `locate_strategy`: " + locate_strategy)

        ask_strategy = np.random.choice(
            list(ask_strategy_weights.keys()),
            p=normalize_1d(list(ask_strategy_weights.values())),
        )
        if ask_strategy == "name":
            query_sparql, verbn = self.asker.ask_name(query_graph, verbn)
        elif ask_strategy == "attr":
            query_sparql, verbn = self.asker.ask_attr(query_graph, verbn)
        else:
            raise Exception("Unexpected `ask_strategy`: " + ask_strategy)

        return query_graph, query_sparql, verbn
