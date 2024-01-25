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
from locate_then_ask.ontokin.mock_entity_store import MockOKEntityStore
from locate_then_ask.ontokin.rxn import OKReactionExampleMaker
from locate_then_ask.ontokin.species import OKSpeciesExampleMaker


SEED_ENTITIES_FILEPATH = "data/seed_entities/ontokin.txt"


class DatasetGenerator:
    @classmethod
    def retrieve_seed_entities(
        cls,
        endpoint: Optional[str],
        user: Optional[str] = None,
        pw: Optional[str] = None,
    ):
        filepath = os.path.join(ROOTDIR, SEED_ENTITIES_FILEPATH)

        if not os.path.isfile(filepath):
            assert (
                endpoint is not None
            ), "No cache of seed entities found, please input endpoint url to query for seed entities."
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
                x["s"]["value"]
                for x in kg_client.query(sparql_rxns)["results"]["bindings"]
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

            entities = dict(Mechanism=mechanisms, Reaction=rxns, Species=species)
            with open(filepath, "w") as f:
                json.dump(entities, f, indent=4)
        else:
            with open(filepath, "r") as f:
                entities = json.load(f)

        return entities

    def __init__(
        self,
        synthetic_abox: bool,
        endpoint: str,
        user: Optional[str] = None,
        pw: Optional[str] = None,
    ):
        if synthetic_abox:
            store = MockOKEntityStore()
        else:
            store = OKEntityStore(endpoint, user=user, pw=pw)
        self.example_maker_mechanism = OKMechanismExampleMaker(store)
        self.example_maker_reaction = OKReactionExampleMaker(store)
        self.example_maker_species = OKSpeciesExampleMaker(store)

        if synthetic_abox:
            seed_entities = dict(
                Mechanism=["placeholder" for _ in range(20)],
                Reaction=["placeholder" for _ in range(40)],
                Species=["placeholder" for _ in range(60)],
            )
        else:
            seed_entities = self.retrieve_seed_entities(endpoint, user, pw)
        self.seed_entities = [
            (concept, x)
            for concept in ["Mechanism", "Reaction", "Species"]
            for x in seed_entities[concept]
        ]
        random.shuffle(self.seed_entities)

    def generate(self, repeats: int = 1):
        examples = []

        for i in tqdm(range(len(self.seed_entities) * repeats)):
            cls, entity_iri = self.seed_entities[i % len(self.seed_entities)]

            if cls == "Mechanism":
                example_maker = self.example_maker_mechanism
                topic_entity = "mechanism"
            elif cls == "Reaction":
                example_maker = self.example_maker_reaction
                topic_entity = "reaction"
            elif cls == "Species":
                example_maker = self.example_maker_species
                topic_entity = "species"
            else:
                raise Exception()

            query_graph, query_sparql, verbalization = example_maker.make_example(
                entity_iri
            )

            example = dict(
                id=i,
                topic_entity=topic_entity,
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
    parser.add_argument(
        "--endpoint",
        default="http://178.128.105.213:3838/blazegraph/namespace/ontokin/sparql",
    )
    parser.add_argument("--user", default=None)
    parser.add_argument("--pw", default=None)
    parser.add_argument("--repeats", type=int, default=1)
    parser.add_argument("--synthetic_abox", action="store_true")
    args = parser.parse_args()

    ds_gen = DatasetGenerator(
        synthetic_abox=args.synthetic_abox,
        endpoint=args.endpoint,
        user=args.user,
        pw=args.pw,
    )
    examples = ds_gen.generate(repeats=args.repeats)

    time_label = time.strftime("%Y-%m-%d_%H.%M.%S")
    filename = "data/ontokin_{timestamp}.json".format(timestamp=time_label)

    with open(os.path.join(ROOTDIR, filename), "w") as f:
        json.dump(examples, f, indent=4)
