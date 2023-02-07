import json
import os
from pprint import pprint

import pandas as pd
from SPARQLWrapper import SPARQLWrapper, JSON
from sklearn.model_selection import train_test_split

from KGToolbox import MakeIndex
from KGToolbox.CreateNegSamplingDictionary import NegSamplingCreator
from Marie.CandidateSelection.location import DATA_DIR
from Marie.Util.CommonTools.FileLoader import FileLoader
from Marie.Util.NHopExtractor import HopExtractor


class OntoSpeciesNewAnalyzer:

    @staticmethod
    def append_to_dict(dictionary, key, value):
        if key in dictionary:
            dictionary[key].append(value)
        else:
            dictionary[key] = [value]
        return dictionary

    def __init__(self, sub_ontology ):
        self.species_role_dictionary, self.role_species_dictionary = self.get_roles_of_species()
        self.species_class_dict, self.class_species_dict = self.get_all_chemical_classes()
        self.ontology = "ontospecies_new"
        self.sub_ontology = sub_ontology
        self.full_dataset_dir = os.path.join(DATA_DIR, "CrossGraph", self.ontology)
        self.sub_ontology_path = os.path.join(self.full_dataset_dir, self.sub_ontology)

    def write_triples_to_tsv(self, all_triples):
        df = pd.DataFrame(all_triples)
        df.to_csv(os.path.join(self.sub_ontology_path, f"{self.sub_ontology}-train.txt"),
                  sep="\t", header=False, index=False)

        df_train, df_test = train_test_split(df, test_size=0.1)
        df_train.to_csv(os.path.join(self.sub_ontology_path, f"{self.sub_ontology}-train-2.txt"),
                        sep="\t", header=False, index=False)

        df_test.to_csv(os.path.join(self.sub_ontology_path, f"{self.sub_ontology}-test.txt"),
                       sep="\t", header=False, index=False)

    def create_triples_with_subclass(self):
        all_triples = []
        for species in self.species_class_dict:
            class_list = self.species_class_dict[species]
            for _class in class_list:
                row = (species, "hasChemicalClass", _class)
                all_triples.append(row)

        for _class in self.class_species_dict:
            species_list = self.class_species_dict[_class]
            for species in species_list:
                row = (_class, "hasInstance", species)
                all_triples.append(row)
        return all_triples

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

        return all_triples

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

    def get_molecular_weight(self):
        """
        Splitted into two things, the node and the value it self
        :return:
        """
        species_node_dict = {}
        node_value_dict = {}
        GET_MOLECULAR_WEIGHT = """
        SELECT DISTINCT  ?species ?node ?value
        WHERE {
            ?species rdf:type  <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#Species> . 	
            ?species <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#hasMolecularWeight> ?node .
            ?o <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#value> ?value
        }  
        """
        rst = self.query_blazegraph(GET_MOLECULAR_WEIGHT)["results"]["bindings"]
        for binding in rst:
            species = binding["species"]["value"].split("/")[-1]
            node = binding["node"]["value"].split("/")[-1]
            value = float(binding["value"]["value"])
            species_node_dict[species] = node
            node_value_dict[node] = value

        print(species_node_dict)
        print(node_value_dict)



    def get_all_chemical_classes(self):
        species_class_dict = {}
        class_species_dict = {}
        GET_CHEMICAL_CLASSES = """
        SELECT DISTINCT ?species  ?types 
        WHERE {
          
          ?species rdf:type  <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#Species> . 	
          ?species <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#hasChemicalClasses>  ?types . 
        } 
        """
        rst = self.query_blazegraph(GET_CHEMICAL_CLASSES)["results"]["bindings"]
        for binding in rst:
            species = binding["species"]["value"].split("/")[-1]
            chemical_classes = binding["types"]["value"].split("/")[-1]
            species_class_dict = self.append_to_dict(species_class_dict, species, chemical_classes)
            class_species_dict = self.append_to_dict(class_species_dict, chemical_classes, species)

        return species_class_dict, class_species_dict

    # def get_all_roles(self):
    #     GET_ALL_ROLES = """
    #     SELECT DISTINCT  ?role ?label
    #     WHERE {
    #
    #         ?role rdf:type  <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#Use> .
    #         ?role rdf:label ?label .
    #     }
    #     """
    #     rst = self.query_blazegraph(GET_ALL_ROLES)["results"]["bindings"]
    #     for binding in rst:
    #         label = binding["label"]["value"]
    #         role = binding["role"]["value"].split("/")[-1]

    def query_blazegraph(self, query, namespace="copy_ontospecies_pubchem_1"):
        sparql = SPARQLWrapper("http://www.theworldavatar.com/blazegraph/namespace/" + namespace + "/sparql")
        sparql.setQuery(query)
        sparql.setReturnFormat(JSON)
        results = sparql.query().convert()
        return results

    def create_inference_candidate_dict(self, entity2idx):
        """
        Make a dictionary mapping true tail to candidate entities in the form of indices
        1. create all uses list
        :return:
        """
        candidate_dict = {}
        role_list = list(self.role_species_dictionary.keys())
        role_list = list(set(role_list))
        role_list = [entity2idx[role] for role in role_list]
        for role_idx in role_list:
            candidate_dict[role_idx] = role_list
        return candidate_dict

    def run(self):
        # self.get_molecular_weight()

        role_triples = self.create_triples_for_roles_of_species()
        chemical_class_triples = self.create_triples_with_subclass()
        all_triples = role_triples + chemical_class_triples
        self.write_triples_to_tsv(all_triples=all_triples)
        ontology = f"{self.ontology}/{self.sub_ontology}"
        MakeIndex.create_indexing(self.sub_ontology, data_dir=f'CrossGraph/{ontology}')
        my_extractor = HopExtractor(dataset_dir=self.sub_ontology_path, dataset_name=self.sub_ontology)
        file_loader = FileLoader(full_dataset_dir=self.sub_ontology_path, dataset_name=self.sub_ontology)
        entity2idx, idx2entity, rel2idx, idx2rel = file_loader.load_index_files()
        candidate_dict = self.create_inference_candidate_dict(entity2idx=entity2idx)
        with open(f"{self.sub_ontology_path}/candidate_dict.json", "w") as f:
            f.write(json.dumps(candidate_dict))
            f.close()
        my_creator = NegSamplingCreator(dataset_dir=self.sub_ontology_path, ontology=self.sub_ontology)
        my_creator.create_full_neg_dictionary()


if __name__ == "__main__":
    my_analyzer = OntoSpeciesNewAnalyzer(sub_ontology="role_with_subclass_mass")
    my_analyzer.run()
