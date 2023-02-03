import os
from pprint import pprint

import pandas as pd
from SPARQLWrapper import SPARQLWrapper, JSON
from sklearn.model_selection import train_test_split

from Marie.CandidateSelection.location import DATA_DIR


class OntoSpeciesNewAnalyzer:

    @staticmethod
    def append_to_dict(dictionary, key, value):
        if key in dictionary:
            dictionary[key].append(value)
        else:
            dictionary[key] = [value]
        return dictionary

    def __init__(self):
        self.species_role_dictionary, self.role_species_dictionary = self.get_roles_of_species()
        self.ontology = "ontospecies_new"
        self.role_only_ontology = "role_only"
        self.full_dataset_dir = os.path.join(DATA_DIR, "CrossGraph", self.ontology)
        self.role_only_path = os.path.join(self.full_dataset_dir, self.role_only_ontology)

    def create_triples_for_roles_of_species(self):
        all_triples = []
        for species in self.species_role_dictionary:
            role_list = self.species_role_dictionary[species]
            for role in role_list:
                row = (species, "hasRole", role)
                all_triples.append(row)

        for role in self.role_species_dictionary:
            species_list = self.role_species_dictionary[role]
            for species in species_list:
                row = (role, "hasSpecies", species)
                all_triples.append(row)

        df = pd.DataFrame(all_triples)
        df.to_csv(os.path.join(self.role_only_path, f"{self.role_only_ontology}-train.txt"),
                  sep="\t", header=False, index=False)

        df_train, df_test = train_test_split(df, test_size=0.1)
        df_train.to_csv(os.path.join(self.role_only_path, f"{self.role_only_ontology}-train-2.txt"),
                        sep="\t", header=False, index=False)

        df_test.to_csv(os.path.join(self.role_only_path, f"{self.role_only_ontology}-test.txt"),
                       sep="\t", header=False, index=False)

    def get_roles_of_species(self):
        species_role_dict = {}
        role_species_dict = {}
        GET_ROLES_OF_SPECIES = """
        SELECT DISTINCT  ?species   ?role
        WHERE {
          
            ?species ?p ?role .
            ?role rdf:type  <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#Use> .
        }
        """
        rst = self.query_blazegraph(GET_ROLES_OF_SPECIES)["results"]["bindings"]
        for binding in rst:
            species = binding["species"]["value"].split("/")[-1]
            role = binding["role"]["value"].split("/")[-1]
            species_role_dict = self.append_to_dict(dictionary=species_role_dict, key=species, value=role)
            role_species_dict = self.append_to_dict(dictionary=role_species_dict, key=role, value=species)

        return species_role_dict, role_species_dict

    def get_all_roles(self):
        GET_ALL_ROLES = """
        SELECT DISTINCT  ?role ?label
        WHERE {
        
            ?role rdf:type  <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#Use> .
            ?role rdf:label ?label .
        } 
        """
        rst = self.query_blazegraph(GET_ALL_ROLES)["results"]["bindings"]
        for binding in rst:
            label = binding["label"]["value"]
            role = binding["role"]["value"].split("/")[-1]

    def query_blazegraph(self, query, namespace="copy_ontospecies_pubchem"):
        sparql = SPARQLWrapper("http://www.theworldavatar.com/blazegraph/namespace/" + namespace + "/sparql")
        sparql.setQuery(query)
        sparql.setReturnFormat(JSON)
        results = sparql.query().convert()
        return results


if __name__ == "__main__":
    my_analyzer = OntoSpeciesNewAnalyzer()
    my_analyzer.create_triples_for_roles_of_species()
