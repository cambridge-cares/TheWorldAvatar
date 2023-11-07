import json
import os
from pathlib import Path
import random

import networkx as nx
from tqdm import tqdm

from locate_then_ask.ontokin.ask.ask_mechanism import OKMechanismAsker
from locate_then_ask.ontokin.ask.ask_rxn import OKReactionAsker
from locate_then_ask.ontokin.ask.ask_species import OKSpeciesAsker
from locate_then_ask.ontokin.entity_store import OKEntityStore
from locate_then_ask.ontokin.locate.locate_mechanism import OKMechanismLocator
from locate_then_ask.ontokin.locate.locate_rxn import OKReactionLocator
from locate_then_ask.ontokin.locate.locate_species import OKSpeciesLocator
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
        mechanisms = [x["s"]["value"] for x in kg_client.query(sparql_mechanisms)["results"]["bindings"]]

        sparql_rxns = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX okin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>

SELECT ?s (COUNT(DISTINCT ?p) as ?degree) WHERE {
  ?s a/rdfs:subClassOf* okin:GasPhaseReaction .
  ?s ?p ?o .
}
GROUP BY ?s
ORDER BY DESC(?degree)
LIMIT 100"""
        rxns = [x["s"]["value"] for x in kg_client.query(sparql_rxns)["results"]["bindings"]]

        sparql_species = """PREFIX okin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>
SELECT ?s (COUNT(DISTINCT ?p) as ?degree) WHERE {
  ?s a okin:Species .
  ?s ?p ?o .
}
GROUP BY ?s
ORDER BY DESC(?degree)
LIMIT 100"""
        species = [x["s"]["value"] for x in kg_client.query(sparql_species)["results"]["bindings"]]

        return mechanisms + rxns + species

    @classmethod
    def retrieve_seed_species(self):
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

        self.mechanism_locator = OKMechanismLocator(self.store)
        self.mechanism_asker = OKMechanismAsker()

        self.rxn_locator = OKReactionLocator(self.store)
        self.rxn_asker = OKReactionAsker()

        self.species_locator = OKSpeciesLocator(self.store)
        self.species_asker = OKSpeciesAsker()

        seed_species = self.retrieve_seed_species()
        random.shuffle(seed_species)
        self.seed_species = seed_species

    def generate(self):
        examples = []

        for id, species in enumerate(tqdm(self.seed_species)):
            cls = self.store.get_cls(species)
            
            if cls == OKMechanism:
                cond_num = random.choice([1, 2, 3])
                query_graph, verbalization = self.mechanism_locator.locate_concept_and_relation_multi(species, cond_num=cond_num)
                ask_datum = self.mechanism_asker.ask_name(query_graph, verbalization)
            
            elif cls == OKGasePhaseReaction:
                locate_strategy = random.choice(["entity_name", "concept_and_relation"])
                if locate_strategy == "entity_name":
                    query_graph, verbalization = self.rxn_locator.locate_entity_name(species)
                    ask_strategies = ["relation"]
                elif locate_strategy == "concept_and_relation":
                    cond_num = random.sample(population=[1, 2, 3, 4], counts=[4, 3, 2, 1], k=1)[0]
                    query_graph, verbalization = self.rxn_locator.locate_concept_and_relation_multi(species, cond_num=cond_num)
                    ask_strategies = ["name", "relation"]
                else:
                    raise Exception()
                
                ask_strategy = random.choice(ask_strategies)
                if ask_strategy == "name":
                    ask_datum = self.rxn_asker.ask_name(query_graph, verbalization)
                elif ask_strategy == "relation":
                    ask_datum = self.rxn_asker.ask_relation(query_graph, verbalization)
                else:
                    raise Exception()
            
            elif cls == OKSpecies:
                locate_strategy = random.choice(["entity_name", "concept_and_relation"])
                if locate_strategy == "entity_name":
                    query_graph, verbalization = self.species_locator.locate_entity_name(species)
                    ask_strategies = ["attribute_or_relation"]
                elif locate_strategy == "concept_and_relation":
                    query_graph, verbalization = self.species_locator.locate_concept_and_relation(species)
                    ask_strategies = ["name", "attribute_or_relation"]
                else:
                    raise Exception()
                
                ask_strategy = random.choice(ask_strategies)
                if ask_strategy == "name":
                    ask_datum = self.species_asker.ask_name(query_graph, verbalization)
                elif ask_strategy == "attribute_or_relation":
                    ask_datum = self.species_asker.ask_attribute_or_relation(query_graph, verbalization)
                else:
                    raise Exception()
            else:
                raise Exception()

            examples.append(dict(
                id=id,
                verbalization=ask_datum.verbalization,
                query=dict(
                    sparql=ask_datum.query_sparql,
                    graph=nx.node_link_data(ask_datum.query_graph)
                )
            ))

        return examples


if __name__ == "__main__":
    ds_gen = DatasetGenerator()
    examples = ds_gen.generate()

    with open(os.path.join(ROOTDIR, "data/ontokin.json"), "w") as f:
        json.dump(examples, f, indent=4)
