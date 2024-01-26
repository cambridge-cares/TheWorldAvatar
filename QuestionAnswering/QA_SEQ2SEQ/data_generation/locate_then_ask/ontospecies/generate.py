from argparse import ArgumentParser
import json
import os
import random
import time

import networkx as nx
from tqdm import tqdm
from constants.fs import ROOTDIR

from constants.ontospecies import SPECIES_ATTRIBUTE_KEYS
from locate_then_ask.ontospecies.ask import OSAsker
from locate_then_ask.ontospecies.locate import OSSpeciesLocator
from locate_then_ask.query_graph import QueryGraph


SEED_SPECIES_NUM = 2000
SEED_SPECIES_FILEPATH = "data/seed_entities/ontospecies.txt"


class DatasetGenerator:
    LOCATE2ASK = {
        "entity_name": (["attribute"], [1]),
        "concept_and_literal": (["name", "attribute"], [3, 1]),
    }

    @classmethod
    def query_seed_species(self, kg_endpoint: str):
        from locate_then_ask.kg_client import KgClient

        kg_client = KgClient(kg_endpoint)

        species_attr_values = "\n        " + "\n        ".join(
            ["(os:has{p})".format(p=p) for p in SPECIES_ATTRIBUTE_KEYS]
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
        return [x["x"]["value"] for x in bindings]

    @classmethod
    def retrieve_seed_species(self, kg_endpoint: str):
        filepath = os.path.join(ROOTDIR, SEED_SPECIES_FILEPATH)

        if not os.path.isfile(filepath):
            print("No seed species found. Retrieving seed species...")
            species = self.query_seed_species(kg_endpoint)
            with open(filepath, "w") as f:
                f.write("\n".join(species))
            print("Retrieval of seed species done.")

        with open(filepath, "r") as f:
            seed_entities = [x.strip() for x in f.readlines()]

        return [x for x in seed_entities if x]

    def __init__(self, kg_endpoint: str):
        self.locator = OSSpeciesLocator(kg_endpoint)
        self.asker = OSAsker()

        self.seed_species = self.retrieve_seed_species(kg_endpoint)
        random.shuffle(self.seed_species)

    def locate(self, locate_strategy: str, species_id: int):
        if locate_strategy == "entity_name":
            entity_num = min(
                random.sample(population=[1, 2, 3], counts=[6, 2, 1], k=1)[0],
                len(self.seed_species) - species_id,
            )
            entity_iris = self.seed_species[species_id : species_id + entity_num]

            query_graph, verbalization = self.locator.locate_entity_name(entity_iris)
            species_id_new = species_id + entity_num
        elif locate_strategy == "concept_and_literal":
            entity_iri = self.seed_species[species_id]
            cond_num = random.sample(
                population=[1, 2, 3, 4, 5, 6], counts=[4, 5, 6, 3, 2, 1], k=1
            )[0]

            query_graph, verbalization = self.locator.locate_concept_and_literal_multi(
                entity_iri, cond_num=cond_num
            )
            species_id_new = species_id + 1
        else:
            raise ValueError("Unrecognized locate strategy: " + locate_strategy)

        return query_graph, verbalization, species_id_new

    def ask(self, ask_strategy: str, query_graph: QueryGraph, verbalization: str):
        if ask_strategy == "name":
            return self.asker.ask_name(query_graph, verbalization)
        elif ask_strategy == "attribute":
            attr_num = random.sample(population=[1, 2, 3], counts=[3, 1, 1], k=1)[0]
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
                query_graph, verbalization, species_id_new = self.locate(
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
                ask_strategy = random.sample(
                    population=ask_strategy_population, counts=ask_strategy_counts, k=1
                )[0]
                ask_datum = self.ask(
                    ask_strategy=ask_strategy,
                    query_graph=query_graph,
                    verbalization=verbalization,
                )

                example = dict(
                    id=example_id,
                    verbalization=ask_datum.verbalization,
                    query=dict(
                        sparql=ask_datum.query_sparql,
                        graph=nx.node_link_data(ask_datum.query_graph),
                    ),
                )
                examples.append(example)
                example_id += 1

                pbar.update(species_id_new - species_id)
                species_id = species_id_new

        return examples


if __name__ == "__main__":
    parser = ArgumentParser()
    parser.add_argument("--kg_endpoint", type=str, required=True)
    args = parser.parse_args()

    ds_gen = DatasetGenerator(args.kg_endpoint)
    examples = ds_gen.generate()

    time_label = time.strftime("%Y-%m-%d_%H.%M.%S")
    filename = "data/ontospecies_{timestamp}.json".format(timestamp=time_label)

    with open(os.path.join(ROOTDIR, filename), "w") as f:
        json.dump(examples, f, indent=4)
