from argparse import ArgumentParser
import json
import os
import time
from typing import Optional

import networkx as nx
import pandas as pd
from tqdm import tqdm
from locate_then_ask.ontozeolite.framework import OZFrameworkExampleMaker

from locate_then_ask.ontozeolite.entity_store import OZEntityStore
from locate_then_ask.ontozeolite.material import OZMaterialExampleMaker
from utils.json import EnumEncoder
from constants.fs import ROOTDIR


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

    def __init__(self, kg_endpoint: str):
        store = OZEntityStore(kg_endpoint)
        self.framework_example_maker = OZFrameworkExampleMaker(store)
        self.material_example_maker = OZMaterialExampleMaker(store)
        self.seed_entities = self.retrieve_seed_entities(kg_endpoint).sample(frac=1)

    def generate(self, n_repeats: int = 1):
        examples = []

        for i, (concept, iri) in enumerate(
            tqdm(
                zip(
                    self.seed_entities["concept"] * n_repeats,
                    self.seed_entities["iri"] * n_repeats,
                )
            )
        ):
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
    parser.add_argument("--n_repeats", type=int, default=1)
    args = parser.parse_args()

    ds_gen = OZDatasetGenerator(args.kg_endpoint)
    examples = []
    examples.extend(ds_gen.generate(args.n_repeats))

    time_label = time.strftime("%Y-%m-%d_%H.%M.%S")
    filename = "data/ontozeolite_{timestamp}.json".format(timestamp=time_label)

    with open(os.path.join(ROOTDIR, filename), "w") as f:
        json.dump(examples, f, indent=4, cls=EnumEncoder)
