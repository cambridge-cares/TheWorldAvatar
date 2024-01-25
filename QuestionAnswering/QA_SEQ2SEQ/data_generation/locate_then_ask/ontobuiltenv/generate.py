from argparse import ArgumentParser
import json
import os
import random
import time
from typing import List

import networkx as nx
import numpy as np
from tqdm import tqdm
from constants.fs import ROOTDIR
from constants.ontobuiltenv import OBE_PROPERTYUSAGE_LABELS, OBEAttrKey
from locate_then_ask.query_graph import QueryGraph
from utils.numerical import normalize_1d

from .ask import OBEAsker
from .locate import OBELocator
from .mock_entity_store import MockOBEEntityStore
from utils.json import EnumEncoder


class OBEDatasetGenerator:
    SEED_ENTITIES_FILEPATH = os.path.join(
        ROOTDIR, "data/seed_entities/ontobuiltenv.txt"
    )

    ASK2WEIGHT = {
        "name": 1,
        "count": 1,  # 1 agg (count)
        "attribute": 7,  # 11 keys
        "agg": 4,  # 3 agg (min, max, avg) + 3 keys (numerical)
        "name_byExtremeAttr": 3,  # 2 agg (min, max) + 3 keys (numerical)
        "attr_byEntityFreq": 6,  # 3 agg (min, max, count) + 6 keys (discrete)
        "attr_byExtremeAttr": 8,  # 2 agg (min, max) + 11 keys
        "discreteAttr_byExtremeAvgAttr": 8,  # 3 agg (min, max, agg) + 9 keys (discrete, numerical)
    }

    @classmethod
    def _retrieve_seed_entities(cls):
        if not os.path.isfile(cls.SEED_ENTITIES_FILEPATH):
            print("No seed entities found. Retrieving seed entities...")
            from locate_then_ask.kg_client import KgClient

            kg_client = KgClient(
                "http://165.232.172.16:3838/blazegraph/namespace/kingslynn/sparql"
            )

            iris: List[str] = []

            query_template_by_use = """PREFIX dabgeo: <http://www.purl.org/oema/infrastructure/>
PREFIX obe: <https://www.theworldavatar.com/kg/ontobuiltenv/>

SELECT DISTINCT ?x (COUNT(DISTINCT ?p) as ?degree) WHERE {{
    ?x a/rdfs:subClassOf* obe:Property ;
       obe:hasPropertyUsage/a obe:{PropertyUsage} ;
       ?p ?o .
}}
GROUP BY ?x
ORDER BY DESC(?degree)
LIMIT {num}"""

            for use in OBE_PROPERTYUSAGE_LABELS.keys():
                query = query_template_by_use.format(PropertyUsage=use, num=20)
                bindings = kg_client.query(query)["results"]["bindings"]
                iris.extend(x["x"]["value"] for x in bindings)

            query_template_by_type = """PREFIX obe: <https://www.theworldavatar.com/kg/ontobuiltenv/>

SELECT DISTINCT ?x (COUNT(DISTINCT ?p) as ?degree) WHERE {{
    ?x a {type} ;
       ?p ?o .
}}
GROUP BY ?x
ORDER BY DESC(?degree)
LIMIT {num}"""

            for concept, num in (("obe:Flat", 60), ("obe:Property", 40)):
                query = query_template_by_type.format(type=concept, num=num)
                bindings = kg_client.query(query)["results"]["bindings"]
                iris.extend(x["x"]["value"] for x in bindings)

            entities = tuple(iris)
            with open(cls.SEED_ENTITIES_FILEPATH, "w") as f:
                f.write("\n".join(entities))
            print("Retrieval done.")
        else:
            with open(cls.SEED_ENTITIES_FILEPATH, "r") as f:
                entities = [x.strip() for x in f.readlines()]

        return [x for x in entities if x]

    def __init__(self, synthetic_abox: bool = False):
        self.locator = OBELocator(MockOBEEntityStore() if synthetic_abox else None)
        self.asker = OBEAsker()

        if synthetic_abox:
            self.seed_entities = ["placeholder" for _ in range(100)]
        else:
            self.seed_entities = self._retrieve_seed_entities()
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
                "name_byExtremeAttr",
                "attr_byEntityFreq",
                "attr_byExtremeAttr",
                "discreteAttr_byExtremeAvgAttr",
            ]
        elif locate_strategy == "concept_and_literal":
            ask_strategies = ["name", "count", "attribute"]

            sampled_attr_keys = tuple(
                key for _, key in query_graph.nodes(data="key") if key is not None
            )
            unsampled_numerical_keys = [
                k not in sampled_attr_keys for k in OBEAsker.NUMERICAL_KEYS
            ]
            if unsampled_numerical_keys:
                ask_strategies.extend(["agg", "name_byExtremeAttr"])
                if len(unsampled_numerical_keys) > 2 or any(
                    k not in sampled_attr_keys for k in OBEAttrKey
                ):
                    ask_strategies.append("attr_byExtremeAttr")
                if any(k not in sampled_attr_keys for k in OBEAsker.DISCRETE_ATTRS):
                    ask_strategies.append("discreteAttr_byExtremeAvgAttr")
            if any(k not in sampled_attr_keys for k in OBEAsker.DISCRETE_ATTRS):
                ask_strategies.append("attr_byEntityFreq")
        else:
            raise ValueError("Unexpected locate strategy: " + locate_strategy)
        return ask_strategies

    def _ask(self, query_graph: QueryGraph, verbn: str, ask_strategy: str):
        if ask_strategy == "name":
            query_sparql, verbn = self.asker.ask_name(query_graph, verbn)
        elif ask_strategy == "count":
            query_sparql, verbn = self.asker.ask_count(query_graph, verbn)
        elif ask_strategy == "attribute":
            attr_num = np.random.choice([1, 2, 3], p=normalize_1d([3, 2, 1]))
            query_sparql, verbn = self.asker.ask_attrs(
                query_graph, verbn, attr_num=attr_num
            )
        elif ask_strategy == "agg":
            attr_num = np.random.choice([1, 2, 3], p=normalize_1d([3, 2, 1]))
            query_sparql, verbn = self.asker.ask_agg(query_graph, verbn, attr_num)
        elif ask_strategy == "name_byExtremeAttr":
            limit = random.randrange(1, 100)
            query_sparql, verbn = self.asker.ask_name_byExtremeAttr(
                query_graph, verbn, limit
            )
        elif ask_strategy == "attr_byEntityFreq":
            limit = random.randrange(1, 100)
            query_sparql, verbn = self.asker.ask_attr_byEntityFreq(
                query_graph, verbn, limit
            )
        elif ask_strategy == "attr_byExtremeAttr":
            limit = random.randrange(1, 100)
            query_sparql, verbn = self.asker.ask_attr_byExtremeAttr(
                query_graph, verbn, limit=limit
            )
        elif ask_strategy == "discreteAttr_byExtremeAvgAttr":
            limit = random.randrange(1, 100)
            query_sparql, verbn = self.asker.ask_discreteAttr_byExtremeAvgAttr(
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
                domain="ontobuiltenv",
                verbalization=verbn,
                query=dict(
                    sparql=query_sparql,
                    graph=nx.node_link_data(query_graph),
                ),
            )
            examples.append(example)

        return examples


if __name__ == "__main__":
    parser = ArgumentParser()
    parser.add_argument("--n_repeats", type=int, default=1)
    parser.add_argument("--synthetic_abox", action="store_true")
    args = parser.parse_args()

    ds_gen = OBEDatasetGenerator(synthetic_abox=args.synthetic_abox)
    examples = []
    examples.extend(ds_gen.generate(args.n_repeats))

    time_label = time.strftime("%Y-%m-%d_%H.%M.%S")
    filename = "data/ontobuiltenv_{timestamp}.json".format(timestamp=time_label)

    with open(os.path.join(ROOTDIR, filename), "w") as f:
        json.dump(examples, f, indent=4, cls=EnumEncoder)
