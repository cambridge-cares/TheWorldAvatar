from argparse import ArgumentParser
import json
import os
import random
import time

import networkx as nx
import numpy as np
from tqdm import tqdm
from constants.fs import ROOTDIR
from constants.ontospecies import OSPropertyKey

from locate_then_ask.ontospecies.ask import OSAsker
from locate_then_ask.ontospecies.locate import OSSpeciesLocator
from locate_then_ask.ontospecies.mock_entity_store import MockOSEntityStore
from locate_then_ask.query_graph import QueryGraph
from utils.json import EnumEncoder
from utils.numerical import normalize_1d


SEED_SPECIES_NUM = 1000
SEED_SPECIES_FILEPATH = "data/seed_entities/ontospecies.txt"


class DatasetGenerator:
    LOCATE2ASK = {
        "entity_name": (["attribute"], [1]),
        "concept_and_literal": (["name", "attribute"], [3, 1]),
    }

    @classmethod
    def _retrieve_seed_species(self):
        filepath = os.path.join(ROOTDIR, SEED_SPECIES_FILEPATH)

        if not os.path.isfile(filepath):
            print("No seed species found. Retrieving seed species...")
            from locate_then_ask.kg_client import KgClient

            kg_client = KgClient(
                "http://178.128.105.213:3838/blazegraph/namespace/ontospecies/sparql"
            )

            species_attr_values = "\n        " + "\n        ".join(
                ["(os:has{p})".format(p=p.value) for p in OSPropertyKey]
            )
            query_template = """PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>

    SELECT DISTINCT ?x (COUNT(DISTINCT ?p) as ?degree) WHERE {{
        ?x a os:Species .
        VALUES (?p) {{{bindings}
        }}
        ?x ?p ?o .
    }}
    GROUP BY ?x
    ORDER BY DESC(?degree)
    LIMIT {num}"""
            query = query_template.format(
                bindings=species_attr_values, num=SEED_SPECIES_NUM
            )
            bindings = kg_client.query(query)["results"]["bindings"]
            entities = [x["x"]["value"] for x in bindings]

            with open(filepath, "w") as f:
                f.write("\n".join(entities))

            print("Retrieval of seed species done.")

        else:
            with open(filepath, "r") as f:
                entities = [x.strip() for x in f.readlines()]

        return [x for x in entities if x]

    def __init__(self, synthetic_abox: bool = False):
        self.locator = OSSpeciesLocator(MockOSEntityStore() if synthetic_abox else None)
        self.asker = OSAsker()

        if synthetic_abox:
            self.seed_species = ["placeholder" for _ in range(1000)]
        else:
            self.seed_species = self._retrieve_seed_species()
        random.shuffle(self.seed_species)

    def _locate(self, locate_strategy: str, species_id: int):
        if locate_strategy == "entity_name":
            entity_num = min(
                np.random.choice([1, 2, 3], p=normalize_1d([16, 4, 1])),
                len(self.seed_species) - species_id,
            )
            entity_iris = self.seed_species[species_id : species_id + entity_num]

            query_graph, verbalization = self.locator.locate_entity_name(entity_iris)
            species_id_new = species_id + entity_num
        elif locate_strategy == "concept_and_literal":
            entity_iri = self.seed_species[species_id]
            cond_num = random.sample(
                population=[1, 2, 3, 4, 5], counts=[4, 8, 4, 2, 1], k=1
            )[0]

            query_graph, verbalization = self.locator.locate_concept_and_literal_multi(
                entity_iri, cond_num=cond_num
            )
            species_id_new = species_id + 1
        else:
            raise ValueError("Unrecognized locate strategy: " + locate_strategy)

        return query_graph, verbalization, species_id_new

    def _ask(self, ask_strategy: str, query_graph: QueryGraph, verbalization: str):
        if ask_strategy == "name":
            return self.asker.ask_name(query_graph, verbalization)
        elif ask_strategy == "attribute":
            attr_num = np.random.choice([1, 2, 3], p=normalize_1d([16, 4, 1]))
            return self.asker.ask_attribute(
                query_graph=query_graph, verbalization=verbalization, attr_num=attr_num
            )
        else:
            raise ValueError("Unrecognized ask_strategy: " + ask_strategy)

    def generate(self):
        species_id = 0

        examples = []
        example_id = 0

        with tqdm(total=len(self.seed_species)) as pbar:
            while species_id < len(self.seed_species):
                locate_strategy = random.choice(list(self.LOCATE2ASK.keys()))
                query_graph, verbalization, species_id_new = self._locate(
                    locate_strategy=locate_strategy, species_id=species_id
                )

                if query_graph is None:
                    print(
                        "Unable to locate a subgraph with the following entity: "
                        + str(self.seed_species[species_id:species_id_new])
                    )
                    continue

                ask_strategy_population, ask_strategy_counts = self.LOCATE2ASK[
                    locate_strategy
                ]
                ask_strategy = np.random.choice(
                    ask_strategy_population, p=normalize_1d(ask_strategy_counts)
                )
                query_sparql, verbalization = self._ask(
                    ask_strategy=ask_strategy,
                    query_graph=query_graph,
                    verbalization=verbalization,
                )

                example = dict(
                    id=example_id,
                    verbalization=verbalization,
                    query=dict(
                        sparql=query_sparql,
                        graph=nx.node_link_data(query_graph),
                    ),
                )
                examples.append(example)
                example_id += 1

                pbar.update(species_id_new - species_id)
                species_id = species_id_new

        return examples


if __name__ == "__main__":
    parser = ArgumentParser()
    parser.add_argument("--synthetic_abox", action="store_true")
    args = parser.parse_args()

    ds_gen = DatasetGenerator(args.synthetic_abox)
    examples = ds_gen.generate()

    time_label = time.strftime("%Y-%m-%d_%H.%M.%S")
    filename = "data/ontospecies_{timestamp}.json".format(timestamp=time_label)

    with open(os.path.join(ROOTDIR, filename), "w") as f:
        json.dump(examples, f, indent=4, cls=EnumEncoder)
