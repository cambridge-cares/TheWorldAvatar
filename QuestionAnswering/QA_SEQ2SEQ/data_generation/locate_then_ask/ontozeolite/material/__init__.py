import numpy as np

from utils.numerical import normalize_1d
from locate_then_ask.make_example_base import ExampleMakerBase
from locate_then_ask.ontozeolite.entity_store import OZEntityStore
from .locate import OZMaterialLocator
from .ask import OZMaterialAsker


class OZMaterialExampleMaker(ExampleMakerBase):
    def __init__(self, store: OZEntityStore):
        self.locator = OZMaterialLocator(store)
        self.asker = OZMaterialAsker()

    def make_example(self, entity_iri: str):
        locate_strategy = np.random.choice(["name", "attr"], p=normalize_1d([1, 2]))
        if locate_strategy == "name":
            query_graph, verbn = self.locator.locate_name(entity_iri)
            ask_strategy_weights = {"attr": 1}
        elif locate_strategy == "attr":
            cond_num = np.random.choice([1, 2, 3, 4], p=normalize_1d([3, 4, 2, 1]))
            query_graph, verbn = self.locator.locate_concept_and_literal_multi(
                entity_iri, cond_num=cond_num
            )

            if self.asker.get_unsampled_keys(query_graph):
                ask_strategy_weights = {"name": 1, "attr": 1}
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
            query_sparql, verbn = self.asker.ask_attr(
                query_graph, verbn, no_ask_guest=locate_strategy == "name"
            )
        else:
            raise Exception("Unexpected `ask_strategy`: " + ask_strategy)

        return query_graph, query_sparql, verbn
