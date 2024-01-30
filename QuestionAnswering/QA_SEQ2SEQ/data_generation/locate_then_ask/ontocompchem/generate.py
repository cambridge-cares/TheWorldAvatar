import argparse
import json
import os
import random
import time

from tqdm import tqdm
import networkx as nx

from constants.fs import ROOTDIR
from locate_then_ask.ontocompchem.ask import OCCAsker
from locate_then_ask.ontocompchem.locate import OCCLocator
from locate_then_ask.ontocompchem.mock_entity_store import MockOCCEntityStore
from utils.json import EnumEncoder


SEED_SPECIES_PATH = "data/seed_entities/ontocompchem.txt"


class DatasetGenerator:
    @classmethod
    def retrieve_seed_entities(cls):
        filepath = os.path.join(ROOTDIR, SEED_SPECIES_PATH)

        if not os.path.isfile(filepath):
            from locate_then_ask.kg_client import KgClient

            kg_client = KgClient(
                "http://178.128.105.213:3838/blazegraph/namespace/ontocompchem/sparql"
            )
            query = """SELECT DISTINCT ?Species WHERE {
    ?MolecularComputation occ:hasSpeciesModel/occ:hasSpecies ?Species
}"""
            response_bindings = kg_client.query(query)["results"]["bindings"]
            seed_entities = [x["Species"]["value"] for x in response_bindings]

            with open(filepath, "w") as f:
                f.write("\n".join(seed_entities))
        else:
            with open(filepath, "r") as f:
                seed_entities = [x.strip() for x in f.readlines()]
                seed_entities = [x for x in seed_entities if x]

        return seed_entities

    def __init__(self, synthetic_abox: bool):
        self.locator = OCCLocator(MockOCCEntityStore() if synthetic_abox else None)
        self.asker = OCCAsker()
        if synthetic_abox:
            self.seed_entities = ["placeholder" for _ in range(100)]
        else:
            self.seed_entities = self.retrieve_seed_entities()
            random.shuffle(self.seed_entities)

    def generate(self, repeats: int = 1):
        examples = []

        for i in tqdm(range(len(self.seed_entities) * repeats)):
            entity_iri = self.seed_entities[i % len(self.seed_entities)]
            query_graph, verbalization = self.locator.locate(entity_iri)

            attr_num = random.sample(population=[1, 2, 3], counts=[16, 4, 1], k=1)[0]
            query_sparql, verbalization = self.asker.ask(
                query_graph=query_graph, verbalization=verbalization, attr_num=attr_num
            )

            example = dict(
                id=i,
                verbalization=verbalization,
                query=dict(
                    sparql=query_sparql,
                    graph=nx.node_link_data(query_graph),
                ),
            )

            examples.append(example)

        return examples


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--n_repeats", type=int, default=1)
    parser.add_argument("--synthetic_abox", action="store_true")
    args = parser.parse_args()

    ds_gen = DatasetGenerator(synthetic_abox=args.synthetic_abox)
    examples = ds_gen.generate(repeats=args.n_repeats)

    time_label = time.strftime("%Y-%m-%d_%H.%M.%S")
    filename = "data/ontocompchem_{timestamp}.json".format(timestamp=time_label)

    with open(os.path.join(ROOTDIR, filename), "w") as f:
        json.dump(examples, f, indent=4, cls=EnumEncoder)
