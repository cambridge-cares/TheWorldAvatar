from argparse import ArgumentParser
import json
import os
import time
from typing import Optional

import networkx as nx
import numpy as np
import pandas as pd
from tqdm import tqdm

from constants.fs import ROOTDIR
from utils.json import EnumEncoder
from .framework import OZFrameworkExampleMaker
from .entity_store import OZEntityStore
from .material import OZMaterialExampleMaker
from .mock import MockOZEntityStore


class OZDatasetGenerator:
    SEED_ENTITIES_FILEPATH = os.path.join(ROOTDIR, "data/seed_entities/ontozeolite.csv")

    @classmethod
    def retrieve_seed_entities(cls, kg_endpoint: Optional[str] = None):
        if not os.path.isfile(cls.SEED_ENTITIES_FILEPATH):
            if kg_endpoint is None:
                raise ValueError(
                    "No cached seed entities found, kg_endpoint must not be None"
                )

            print("No seed entities found. Retrieving seed entities...")
            from locate_then_ask.kg_client import KgClient

            kg_client = KgClient(kg_endpoint)

            template = """PREFIX zeo: <http://www.theworldavatar.com/kg/ontozeolite/>

SELECT DISTINCT ?s
WHERE {{
  ?s a {cls}
}}
LIMIT 100"""
            data = dict()
            for key, clsname in [
                ("Framework", "zeo:ZeoliteFramework"),
                ("Material", "zeo:ZeoliticMaterial"),
            ]:
                bindings = kg_client.query(template.format(cls=clsname))["results"][
                    "bindings"
                ]
                data[key] = [x["s"]["value"] for x in bindings]

            data = [
                dict(concept=key, iri=iri) for key, iris in data.items() for iri in iris
            ]
            df = pd.DataFrame(data)
            df.to_csv(cls.SEED_ENTITIES_FILEPATH)
            print("Retrieval done.")
        else:
            df = pd.read_csv(cls.SEED_ENTITIES_FILEPATH)

        return df

    def __init__(self, kg_endpoint: Optional[str] = None, synthetic_abox: bool = False):
        if synthetic_abox:
            store = MockOZEntityStore(kg_endpoint)
            seed_entities = pd.DataFrame(
                data=dict(
                    concept=["Framework"] * 100 + ["Material"] * 100,
                    iri=["placeholder"] * 200,
                )
            )
        else:
            store = OZEntityStore(kg_endpoint)
            seed_entities = self.retrieve_seed_entities(kg_endpoint)

        self.framework_example_maker = OZFrameworkExampleMaker(store)
        self.material_example_maker = OZMaterialExampleMaker(store)
        self.seed_entities = seed_entities.sample(frac=1)

    def generate(self, n_repeats: int = 1):
        examples = []

        concepts = self.seed_entities["concept"].repeat(n_repeats)
        iris = self.seed_entities["iri"].repeat(n_repeats)
        ids = np.arange(len(concepts))
        np.random.shuffle(ids)
        for i, (concept, iri) in enumerate(tqdm(zip(concepts.iloc[ids], iris.iloc[ids]))):
            if concept == "Framework":
                func = self.framework_example_maker.make_example
            elif concept == "Material":
                func = self.material_example_maker.make_example
            else:
                raise ValueError("Unexpected concept: " + concept)

            query_graph, query_sparql, verbn = func(iri)

            example = dict(
                id=i,
                domain="ontozeolite",
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
    parser.add_argument("--kg_endpoint", type=str, default=None)
    parser.add_argument("--synthetic_abox", action="store_true")
    parser.add_argument("--n_repeats", type=int, default=1)
    args = parser.parse_args()

    ds_gen = OZDatasetGenerator(args.kg_endpoint, args.synthetic_abox)
    examples = []
    examples.extend(ds_gen.generate(args.n_repeats))

    time_label = time.strftime("%Y-%m-%d_%H.%M.%S")
    filename = "data/ontozeolite_{timestamp}.json".format(timestamp=time_label)

    with open(os.path.join(ROOTDIR, filename), "w") as f:
        json.dump(examples, f, indent=4, cls=EnumEncoder)
