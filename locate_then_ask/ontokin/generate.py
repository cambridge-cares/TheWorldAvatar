import json
import os
from pathlib import Path
import time

import networkx as nx
from tqdm import tqdm
from locate_then_ask.ontokin.entity_store import OKEntityStore

from locate_then_ask.ontokin.mechanism import OKMechanismExampleMaker
from locate_then_ask.ontokin.rxn import OKReactionExampleMaker
from locate_then_ask.ontokin.species import OKSpeciesExampleMaker
from locate_then_ask.ontokin.model import OKGasePhaseReaction, OKMechanism, OKSpecies


ROOTDIR = Path(os.getcwd())
SEED_SPECIES_NUM = 2000
SEED_SPECIES_FILEPATH = "data/seed_entities/ontokin.txt"


class DatasetGenerator:
    @classmethod
    def query_seed_entities(self):
        from locate_then_ask.kg_client import KgClient

        kg_client = KgClient(
            "http://theworldavatar.com/blazegraph/namespace/ontokin/sparql"
        )

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

        sparql_species = """PREFIX okin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>
SELECT ?s (COUNT(DISTINCT ?p) as ?degree) WHERE {
  ?s a okin:Species .
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
    def retrieve_seed_entities(self):
        filepath = os.path.join(ROOTDIR, SEED_SPECIES_FILEPATH)

        if not os.path.isfile(os.path.join(filepath)):
            entities = self.query_seed_entities()
            with open(filepath, "w") as f:
                f.write("\n".join(entities))

        with open(filepath, "r") as f:
            seed_entities = [x.strip() for x in f.readlines()]

        return [x for x in seed_entities if x]

    def __init__(self):
        self.store = OKEntityStore()
        self.example_maker_mechanism = OKMechanismExampleMaker(self.store)
        self.example_maker_reaction = OKReactionExampleMaker(self.store)
        self.example_maker_species = OKSpeciesExampleMaker(self.store)

        self.seed_entities = self.retrieve_seed_entities()
        # random.shuffle(self.seed_entities)

    def generate(self):
        examples = []

        for id, entity_iri in enumerate(tqdm(self.seed_entities)):
            cls = self.store.get_cls(entity_iri)

            if cls == OKMechanism:
                example_maker = self.example_maker_mechanism
                topic_entity = "mechanism"
            elif cls == OKGasePhaseReaction:
                example_maker = self.example_maker_reaction
                topic_entity = "reaction"
            elif cls == OKSpecies:
                example_maker = self.example_maker_species
                topic_entity = "species"
            else:
                raise Exception()

            ask_datum = example_maker.make_example(entity_iri)

            example = dict(
                id=id,
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
    ds_gen = DatasetGenerator()
    examples = ds_gen.generate()

    time_label = time.strftime("%Y-%m-%d_%H.%M.%S")
    filename = "data/ontokin_{timestamp}.json".format(timestamp=time_label)

    with open(os.path.join(ROOTDIR, filename), "w") as f:
        json.dump(examples, f, indent=4)
