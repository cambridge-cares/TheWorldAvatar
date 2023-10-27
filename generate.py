import json
import os
from pathlib import Path
import random

import networkx as nx
from tqdm import tqdm

from constants.ontospecies_keys import SPECIES_ATTRIBUTE_KEYS
from locate_then_ask.ask import Asker
from locate_then_ask.locate import Locator


ROOTDIR = Path(os.getcwd())


def query_seed_species():
    from locate_then_ask.kg_client import KgClient

    kg_client = KgClient(
        "http://178.128.105.213:3838/blazegraph/namespace/ontospecies/sparql"
    )

    species_attr_values = "\n    ".join(
        ["(os:has{p})".format(p=p) for p in SPECIES_ATTRIBUTE_KEYS]
    )
    query = """"PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
SELECT DISTINCT ?x (COUNT(DISTINCT ?p) as ?degree) WHERE {{
    ?x a os:Species .
    VALUES (?p) {{{bindings}
    }}
    ?x ?p ?o .
}}
GROUP BY ?x
ORDER BY DESC(?degree)
LIMIT 100""".format(
        bindings=species_attr_values
    )
    bindings = kg_client.query(query)
    return [x["x"]["value"] for x in bindings]


def retrieve_seed_species():
    filepath = os.path.join(ROOTDIR, "seed_species.txt")

    if not os.path.isfile(os.path.join(filepath)):
        species = query_seed_species()
        with open(filepath, "w") as f:
            f.write("\n".join(species))

    with open(filepath, "r") as f:
        seed_entities = [x.strip() for x in f.readlines()]

    return [x for x in seed_entities if x]


if __name__ == "__main__":
    locator = Locator()
    asker = Asker()

    seed_species = retrieve_seed_species()[:20]
    random.shuffle(seed_species)
    species_id = 0

    examples = []
    example_id = 0

    locate2ask = {
        "entity_name": ["attribute"],
        "concept_and_literal": ["name", "attribute"]
    }

    with tqdm(total=len(seed_species)) as pbar:
        while species_id < len(seed_species):
            locate_strategy = random.choice(list(locate2ask.keys()))
            if locate_strategy == "entity_name":
                entity_num = min(random.sample(population=[1, 2, 3], counts=[3, 2, 1], k=1)[0], len(seed_species) - species_id)
                entity_iris = seed_species[species_id: species_id + entity_num]

                query_graph, verbalization = locator.locate_entity_name(entity_iris)
                species_id_new = species_id + entity_num
            else:
                entity_iri = seed_species[species_id]
                cond_num = random.sample(
                    population=[1, 2, 3, 4, 5], counts=[2, 4, 5, 2, 1], k=1
                )[0]

                query_graph, verbalization = locator.locate_intersection(
                    entity_iri, cond_num=cond_num
                )
                species_id_new = species_id + 1

            if query_graph is None:
                print("Unable to locate a subgraph with the following entity: " + entity_iri)
                continue

            ask_strategy = random.choice(locate2ask[locate_strategy])
            if ask_strategy == "name":
                (
                    query_graph,
                    (sparql_compact, sparql_verbose),
                    verbalization,
                ) = asker.ask_name(query_graph, verbalization)
            elif ask_strategy == "attribute":
                (
                    query_graph,
                    (sparql_compact, sparql_verbose),
                    verbalization,
                ) = asker.ask_attribute(query_graph, verbalization)
            else:
                raise ValueError("Unrecognized ask_strategy: " + ask_strategy)
            
            examples.append(
                dict(
                    id=example_id,
                    verbalization=verbalization,
                    query=dict(
                        sparql_compact=sparql_compact,
                        sparql_verbose=sparql_verbose,
                        graph=nx.node_link_data(query_graph)
                    ),
                )
            )
            example_id += 1
            pbar.update(species_id_new - species_id)
            species_id = species_id_new

    with open(os.path.join(ROOTDIR, "examples.json"), "w") as f:
        json.dump(examples, f, indent=4)
