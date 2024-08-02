from argparse import ArgumentParser
import json
import os
import random
import time
from typing import Optional

import networkx as nx
import numpy as np
from tqdm import tqdm
from locate_then_ask.singapore.mock_entity_store import MockSgEntityStore

from utils.numerical import normalize_1d
from utils.json import EnumEncoder
from constants.fs import ROOTDIR
from constants.plot import OPltPlotAttrKey
from locate_then_ask.query_graph import QueryGraph
from locate_then_ask.singapore.ask import OPltPlotAsker
from locate_then_ask.singapore.entity_store import SgEntityStore
from locate_then_ask.singapore.locate import OPltPlotLocator


class SgDatasetGenerator:
    SEED_ENTITIES_FILEPATH = os.path.join(ROOTDIR, "data/seed_entities/singapore.txt")

    ASK2WEIGHT = {
        "count": 1,
        "attribute": 3,
        "agg": 3,
        "attr_byExtremeAttr": 3,
    }

    @classmethod
    def retrieve_seed_entities(
        cls, bg_endpoint: Optional[str], ontop_endpoint: Optional[str]
    ):
        if not os.path.isfile(cls.SEED_ENTITIES_FILEPATH):
            if bg_endpoint is None:
                raise ValueError("No cached seed entities found, kg_endpoint must not be None")

            print("No seed entities found. Retrieving seed entities...")
            from locate_then_ask.kg_client import KgClient

            kg_client = KgClient(bg_endpoint)

            query_template = """PREFIX oplt: <https://www.theworldavatar.com/kg/ontoplot/>

SELECT DISTINCT ?Plot WHERE {{
    SERVICE <{ontop}> {{
        ?Plot a oplt:Plot
    }}
}}
LIMIT 200"""

            query = query_template.format(ontop=ontop_endpoint)
            bindings = kg_client.query(query)["results"]["bindings"]
            iris = [x["Plot"]["value"] for x in bindings]

            entities = tuple(iris)
            with open(cls.SEED_ENTITIES_FILEPATH, "w") as f:
                f.write("\n".join(entities))
            print("Retrieval done.")
        else:
            with open(cls.SEED_ENTITIES_FILEPATH, "r") as f:
                entities = [x.strip() for x in f.readlines()]

        return [x for x in entities if x]

    def __init__(
        self, bg_endpoint: Optional[str] = None, ontop_endpoint: Optional[str] = None, synthetic_abox: bool = False
    ):
        if synthetic_abox:
            store = MockSgEntityStore(bg_endpoint, ontop_endpoint)
        else:
            store = SgEntityStore(bg_endpoint, ontop_endpoint)
        self.locator = OPltPlotLocator(store)
        self.asker = OPltPlotAsker()

        self.seed_entities = self.retrieve_seed_entities(bg_endpoint, ontop_endpoint)
        random.shuffle(self.seed_entities)

    def _locate(self, entity: str, locate_strategy: str):
        if locate_strategy == "concept_name":
            query_graph, verbn = self.locator.locate_concept_name(entity)
        elif locate_strategy == "concept_and_literal":
            cond_num = np.random.choice([1, 2, 3], p=normalize_1d([1, 2, 3]))
            query_graph, verbn = self.locator.locate_concept_and_literal_multi(
                entity, cond_num=cond_num
            )
        else:
            raise ValueError("Unexpected locate strategy: " + locate_strategy)

        return query_graph, verbn

    def _compute_ask_strategies(self, query_graph: QueryGraph, locate_strategy: str):
        if locate_strategy == "concept_name":
            ask_strategies = [
                "count",
                "agg",
                "attr_byExtremeAttr",
            ]
        elif locate_strategy == "concept_and_literal":
            ask_strategies = ["count", "attribute"]

            sampled_attr_keys = tuple(
                key for _, _, key in query_graph.edges(data="key") if key is not None
            )
            unsampled_numerical_keys = [
                k for k in OPltPlotAsker.NUMERICAL_KEYS if k not in sampled_attr_keys
            ]
            if unsampled_numerical_keys:
                ask_strategies.append("agg")
                if len(unsampled_numerical_keys) >= 2 or any(
                    k not in sampled_attr_keys
                    and k not in unsampled_numerical_keys
                    and k is not OPltPlotAttrKey.IS_AWAITING_DETAILED_GPR_EVAL
                    for k in OPltPlotAttrKey
                ):
                    ask_strategies.append("attr_byExtremeAttr")
        else:
            raise ValueError("Unexpected locate strategy: " + locate_strategy)
        return ask_strategies

    def _ask(self, query_graph: QueryGraph, verbn: str, ask_strategy: str):
        if ask_strategy == "count":
            query_sparql, verbn = self.asker.ask_count(query_graph, verbn)
        elif ask_strategy == "attribute":
            attr_num = np.random.choice([1, 2, 3], p=normalize_1d([3, 2, 1]))
            query_sparql, verbn = self.asker.ask_attrs(
                query_graph, verbn, attr_num=attr_num
            )
        elif ask_strategy == "agg":
            attr_num = np.random.choice([1, 2, 3], p=normalize_1d([3, 2, 1]))
            query_sparql, verbn = self.asker.ask_agg(query_graph, verbn, attr_num)
        elif ask_strategy == "attr_byExtremeAttr":
            limit = random.randrange(1, 100)
            query_sparql, verbn = self.asker.ask_attr_byExtremeAttr(
                query_graph, verbn, limit=limit
            )
        else:
            raise ValueError("Unexpected ask strategy: " + ask_strategy)

        return query_sparql, verbn

    def generate(self, n_repeats: int = 1):
        examples = []

        for i, entity in enumerate(tqdm(self.seed_entities * n_repeats)):
            locate_strategy = np.random.choice(
                ["concept_name", "concept_and_literal"], p=normalize_1d([1, 4])
            )

            query_graph, verbn = self._locate(entity, locate_strategy)
            ask_strategies = self._compute_ask_strategies(query_graph, locate_strategy)

            ask_strategy = np.random.choice(
                ask_strategies,
                p=normalize_1d([self.ASK2WEIGHT[x] for x in ask_strategies]),
            )
            query_sparql, verbn = self._ask(query_graph, verbn, ask_strategy)

            example = dict(
                id=i,
                domain="singapore",
                verbalization=verbn + "?",
                query=dict(
                    sparql=query_sparql,
                    graph=nx.node_link_data(query_graph),
                ),
            )
            examples.append(example)

        return examples


if __name__ == "__main__":
    parser = ArgumentParser()
    parser.add_argument("--bg_endpoint", type=str, default=None)
    parser.add_argument("--ontop_endpoint", type=str, default=None)
    parser.add_argument("--n_repeats", type=int, default=1)
    parser.add_argument("--synthetic_abox", action="store_true", default=False)
    args = parser.parse_args()

    ds_gen = SgDatasetGenerator(
        bg_endpoint=args.bg_endpoint,
        ontop_endpoint=args.ontop_endpoint,
        synthetic_abox=args.synthetic_abox
    )
    examples = []
    examples.extend(ds_gen.generate(args.n_repeats))

    time_label = time.strftime("%Y-%m-%d_%H.%M.%S")
    filename = "data/singapore_{timestamp}.json".format(timestamp=time_label)

    with open(os.path.join(ROOTDIR, filename), "w") as f:
        json.dump(examples, f, indent=4, cls=EnumEncoder)
