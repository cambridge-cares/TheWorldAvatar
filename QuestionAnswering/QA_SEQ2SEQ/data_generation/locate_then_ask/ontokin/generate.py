import argparse
import json
import os
import random
import time
from typing import Optional

import networkx as nx
from tqdm import tqdm
from constants.fs import ROOTDIR
from locate_then_ask.ontokin.entity_store import OKEntityStore

from locate_then_ask.ontokin.mechanism import OKMechanismExampleMaker
from locate_then_ask.ontokin.rxn import OKReactionExampleMaker
from locate_then_ask.ontokin.species import OKSpeciesExampleMaker
from locate_then_ask.ontokin.model import OKGasPhaseReaction, OKMechanism, OKSpecies


SEED_SPECIES_NUM = 2000
SEED_SPECIES_FILEPATH = "data/seed_entities/ontokin.txt"


class DatasetGenerator:
    @classmethod
    def query_seed_entities(
        self, endpoint: str, user: Optional[str] = None, pw: Optional[str] = None
    ):
        from locate_then_ask.kg_client import KgClient

        kg_client = KgClient(endpoint, user=user, pw=pw)

        sparql_mechanisms = """PREFIX okin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>

SELECT * WHERE {
  ?s a okin:ReactionMechanism
}
LIMIT 10"""
        mechanisms = [
            x["s"]["value"]
            for x in kg_client.query(sparql_mechanisms)["results"]["bindings"]
        ]

        sparql_rxns = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX okin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>

SELECT ?s (COUNT(DISTINCT ?p) as ?degree) WHERE {
  ?s a/rdfs:subClassOf* okin:GasPhaseReaction .
  ?s ?p ?o .
}
GROUP BY ?s
ORDER BY DESC(?degree)
LIMIT 100"""
        rxns = [
            x["s"]["value"] for x in kg_client.query(sparql_rxns)["results"]["bindings"]
        ]
        sparql_species = """PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>

SELECT ?s (COUNT(DISTINCT ?p) as ?degree) WHERE {
  ?s a/rdfs:subClassOf* os:Species .
  ?s ?p ?o .
}
GROUP BY ?s
ORDER BY DESC(?degree)
LIMIT 100"""
        species = [
            x["s"]["value"]
            for x in kg_client.query(sparql_species)["results"]["bindings"]
        ]
        return mechanisms + rxns + species

    @classmethod
    def retrieve_seed_entities(
        cls,
        endpoint: Optional[str],
        user: Optional[str] = None,
        pw: Optional[str] = None,
    ):
        filepath = os.path.join(ROOTDIR, SEED_SPECIES_FILEPATH)

        if not os.path.isfile(filepath):
            assert (
                endpoint is not None
            ), "No cache of seed entities found, please input endpoint url to query for seed entities."
            entities = cls.query_seed_entities(endpoint, user, pw)
            with open(filepath, "w") as f:
                f.write("\n".join(entities))

        with open(filepath, "r") as f:
            seed_entities = [x.strip() for x in f.readlines()]

        return [x for x in seed_entities if x]

    def __init__(
        self, endpoint: str, user: Optional[str] = None, pw: Optional[str] = None
    ):
        self.store = OKEntityStore(endpoint, user=user, pw=pw)
        self.example_maker_mechanism = OKMechanismExampleMaker(self.store)
        self.example_maker_reaction = OKReactionExampleMaker(self.store)
        self.example_maker_species = OKSpeciesExampleMaker(self.store)

        self.seed_entities = self.retrieve_seed_entities(endpoint, user, pw)
        random.shuffle(self.seed_entities)

    def generate(self, repeats: int = 1):
        examples = []

        for i in tqdm(range(len(self.seed_entities) * repeats)):
            entity_iri = self.seed_entities[i % len(self.seed_entities)]
            cls = self.store.get_cls(entity_iri)

            if cls == OKMechanism:
                example_maker = self.example_maker_mechanism
                topic_entity = "mechanism"
            elif cls == OKGasPhaseReaction:
                example_maker = self.example_maker_reaction
                topic_entity = "reaction"
            elif cls == OKSpecies:
                example_maker = self.example_maker_species
                topic_entity = "species"
            else:
                raise Exception()

            ask_datum = example_maker.make_example(entity_iri)

            example = dict(
                id=i,
                topic_entity=topic_entity,
                verbalization=ask_datum.verbalization,
                query=dict(
                    sparql=ask_datum.query_sparql,
                    graph=nx.node_link_data(ask_datum.query_graph),
                ),
            )
            examples.append(example)

        return examples


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--kg_endpoint", type=str, required=True)
    parser.add_argument("--user", default=None)
    parser.add_argument("--pw", default=None)
    parser.add_argument("--repeats", type=int, default=1)
    args = parser.parse_args()

    ds_gen = DatasetGenerator(endpoint=args.endpoint, user=args.user, pw=args.pw)
    examples = ds_gen.generate(repeats=args.repeats)

    time_label = time.strftime("%Y-%m-%d_%H.%M.%S")
    filename = "data/ontokin_{timestamp}.json".format(timestamp=time_label)

    with open(os.path.join(ROOTDIR, filename), "w") as f:
        json.dump(examples, f, indent=4)
