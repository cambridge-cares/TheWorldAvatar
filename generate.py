import json
import os
from pathlib import Path
import random

import networkx as nx
from tqdm import tqdm

from constants.ontospecies_keys import SPECIES_ATTRIBUTE_KEYS
from locate_then_ask.ask import AskDatum, Asker
from locate_then_ask.locate import Locator


ROOTDIR = Path(os.getcwd())
SEED_SPECIES_NUM = 2000
SEED_SPECIES_FILEPATH = "data/seed_species_{num}.txt".format(num=SEED_SPECIES_NUM)


class DatasetGenerator:
    LOCATE2ASK = {
        "entity_name": (["attribute"], [1]),
        "concept_and_literal": (["name", "attribute"], [3, 1]),
    }

    @classmethod
    def query_seed_species(self):
        from locate_then_ask.kg_client import KgClient

        kg_client = KgClient(
            "http://178.128.105.213:3838/blazegraph/namespace/ontospecies/sparql"
        )

        species_attr_values = "\n        " + "\n        ".join(
            ["(os:has{p})".format(p=p) for p in SPECIES_ATTRIBUTE_KEYS]
        )
        query_template = '''PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>

SELECT DISTINCT ?x (COUNT(DISTINCT ?p) as ?degree) WHERE {{
    ?x a os:Species .
    VALUES (?p) {{{bindings}
    }}
    ?x ?p ?o .
}}
GROUP BY ?x
ORDER BY DESC(?degree)
LIMIT {num}'''
        query = query_template.format(
            bindings=species_attr_values, num=SEED_SPECIES_NUM
        )
        bindings = kg_client.query(query)
        return [x["x"]["value"] for x in bindings]

    @classmethod
    def retrieve_seed_species(self):
        filepath = os.path.join(ROOTDIR, SEED_SPECIES_FILEPATH)

        if not os.path.isfile(os.path.join(filepath)):
            species = self.query_seed_species()
            with open(filepath, "w") as f:
                f.write("\n".join(species))

        with open(filepath, "r") as f:
            seed_entities = [x.strip() for x in f.readlines()]

        return [x for x in seed_entities if x]

    def __init__(self):
        self.locator = Locator()
        self.asker = Asker()

        seed_species = self.retrieve_seed_species()
        random.shuffle(seed_species)
        self.seed_species = seed_species

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

            query_graph, verbalization = self.locator.locate_intersection(
                entity_iri, cond_num=cond_num
            )
            species_id_new = species_id + 1
        else:
            raise ValueError("Unrecognized locate strategy: " + locate_strategy)

        return query_graph, verbalization, species_id_new

    def ask(self, ask_strategy: str, query_graph: nx.DiGraph, verbalization: str):
        if ask_strategy == "name":
            return self.asker.ask_name(query_graph, verbalization)
        elif ask_strategy == "attribute":
            attr_num = random.sample(population=[1, 2, 3], counts=[3, 1, 1], k=1)[0]
            return self.asker.ask_attribute(
                query_graph=query_graph, verbalization=verbalization, attr_num=attr_num
            )
        else:
            raise ValueError("Unrecognized ask_strategy: " + ask_strategy)

    def make_example(self, example_id: int, ask_datum: AskDatum):
        return dict(
            id=example_id,
            verbalization=ask_datum.verbalization,
            query=dict(
                sparql_compact=ask_datum.query_sparql[0],
                sparql_verbose=ask_datum.query_sparql[1],
                graph=nx.node_link_data(ask_datum.query_graph),
            ),
        )

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
                example = self.make_example(example_id=example_id, ask_datum=ask_datum)
                examples.append(example)
                example_id += 1

                pbar.update(species_id_new - species_id)
                species_id = species_id_new

        return examples


if __name__ == "__main__":
    ds_gen = DatasetGenerator()
    examples = ds_gen.generate()

    with open(os.path.join(ROOTDIR, "examples_{num}.json".format(num=SEED_SPECIES_NUM)), "w") as f:
        json.dump(examples, f, indent=4)
