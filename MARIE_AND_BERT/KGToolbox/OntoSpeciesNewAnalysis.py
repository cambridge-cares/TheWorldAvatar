import json
import os
import random
from pprint import pprint
import sys

sys.path.append("")
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
        if (type(key) == list):
            for k in key:
                if k in dictionary:
                    dictionary[k].append(value)
                else:
                    dictionary[k] = [value]
        else:
            if key in dictionary:
                dictionary[key].append(value)
            else:
                dictionary[key] = [value]
        return dictionary

    def __init__(self, sub_ontology):
        self.species_role_dictionary, self.role_species_dictionary = self.get_roles_of_species()
        self.species_class_dict, self.class_species_dict = self.get_all_chemical_classes()
        self.numerical_attribute_list = ["hasLogP", "hasDensity", "hasBoilingPoint",
                                         "hasSolubility", "hasLogP", "hasLogS", "hasMolecularWeight",
                                         "hasMeltingPoint"]
        self.numerical_attribute_species_node_dict, self.node_value_dict = self.get_all_numerical_attributes()

        self.ontology = "ontospecies_new"
        self.sub_ontology = sub_ontology
        self.full_dataset_dir = os.path.join(DATA_DIR, "CrossGraph", self.ontology)
        self.sub_ontology_path = os.path.join(self.full_dataset_dir, self.sub_ontology)

    def get_all_numerical_attributes(self):
        full_numerical_attribute_species_node_dict = {}
        full_node_value_dict = {}
        for numerical_attribute in self.numerical_attribute_list:
            species_node_dict, node_value_dict = self.numerical_data_from_kg(numerical_attribute_label=
                                                                             numerical_attribute)
            full_numerical_attribute_species_node_dict[numerical_attribute] = species_node_dict
            full_node_value_dict.update(node_value_dict)

        return full_numerical_attribute_species_node_dict, full_node_value_dict

    def numerical_data_from_kg(self, numerical_attribute_label):
        """
        The generic method for querying numerical values from the KG
        :return: species_node_dict, which maps the species IRI to the node IRI
                 node_value_dict, which maps the node IRI to its numerical value
        """
        species_node_dict = {}
        node_value_dict = {}

        NUMERICAL_DATA_QUERY = """
        SELECT DISTINCT  ?species ?node ?value
        WHERE {
            ?species  rdf:type  <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#Species> . 	
            ?species <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#%s> ?node .
            ?node <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#value> ?value
        }  
        """ % numerical_attribute_label
        rst = self.query_blazegraph(NUMERICAL_DATA_QUERY)["results"]["bindings"]
        for binding in rst:
            species = binding["species"]["value"].split("/")[-1]
            node = binding["node"]["value"].split("/")[-1]
            value = float(binding["value"]["value"])
            species_node_dict[species] = node
            node_value_dict[node] = value

        return species_node_dict, node_value_dict

    def write_triples_to_tsv(self, numerical_triples, other_triples):
        # only remove triples with hasRole and hasSpecies
        # 1. sample 200 triples with hasRole and hasSpecies
        # 2. remove the 200 triples from the other_triples
        inference_rels = ["hasRole", "hasSpecies"]
        other_triples_for_inference = [triples for triples in other_triples if triples[1].strip() in inference_rels]
        test_other_triples = random.sample(other_triples_for_inference, 200)
        train_other_triples = [triples for triples in other_triples if triples not in test_other_triples]
        df = pd.DataFrame(numerical_triples + other_triples)
        df.to_csv(os.path.join(self.sub_ontology_path, f"{self.sub_ontology}-train.txt"),
                  sep="\t", header=False, index=False)

        df_train = pd.DataFrame(numerical_triples + train_other_triples)
        df_test = pd.DataFrame(test_other_triples)
        # also make sure the train df doesn't loss any numerical triples
        # df_train, df_test = train_test_split(df, test_size=0.1)

        # remove all the numerical triples from the test set
        # "hasRole", "hasSpecies"
        # df_test = df_test[(df_test.iloc[:, 1] == "hasRole") | (df_test.iloc[:, 1] == "hasSpecies")]

        df_train.to_csv(os.path.join(self.sub_ontology_path, f"{self.sub_ontology}-train-2.txt"),
                        sep="\t", header=False, index=False)

        df_test.to_csv(os.path.join(self.sub_ontology_path, f"{self.sub_ontology}-test.txt"),
                       sep="\t", header=False, index=False)

    def create_numerical_triples(self):
        numerical_triples = []
        numerical_evaluation_triples = []
        for numerical_attribute in self.numerical_attribute_species_node_dict:
            species_node_dict = self.numerical_attribute_species_node_dict[numerical_attribute]
            for species in species_node_dict:
                node = species_node_dict[species]
                value = self.node_value_dict[node]
                row_node = (species, numerical_attribute, node)
                row_value = (species, numerical_attribute, float(value))
                numerical_triples.append(row_node)
                numerical_evaluation_triples.append(row_value)
        return numerical_triples, random.sample(numerical_evaluation_triples, 100)

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

    def get_chemical_class_tree(self, chemical_class):

        query = """
        SELECT DISTINCT ?class   
        WHERE { <""" + chemical_class + """> rdf:type  ?class . 
        } 
        """
        sub_classes = []
        rst = self.query_blazegraph(query)["results"]["bindings"]
        for binding in rst:
            if ("OntoSpecies.owl#ChemicalClass" not in binding["class"]["value"]):
                sub_classes.append(binding["class"]["value"])

        return sub_classes

    def get_all_chemical_classes(self):
        species_class_dict = {}
        class_species_dict = {}
        GET_CHEMICAL_CLASSES = """
        SELECT DISTINCT ?species  ?types 
        WHERE {
          
          ?species rdf:type  <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#Species> . 	
          ?species <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#hasChemicalClass>  ?types . 
        } 
        """

        rst = self.query_blazegraph(GET_CHEMICAL_CLASSES)["results"]["bindings"]
        for binding in rst:
            chemical_class_list = []

            species = binding["species"]["value"].split("/")[-1]
            chemical_classes = binding["types"]["value"]

            chemical_class_list.append(chemical_classes)

            sub_classes = self.get_chemical_class_tree(chemical_classes)
            chemical_class_list.extend(sub_classes)

            for c in sub_classes:
                chemical_class_list.extend(self.get_chemical_class_tree(c))

            for i, c in enumerate(chemical_class_list):
                chemical_class_list[i] = c.split("/")[-1]

            species_class_dict = self.append_to_dict(species_class_dict, species, chemical_class_list)
            class_species_dict = self.append_to_dict(class_species_dict, chemical_class_list, species)

        return species_class_dict, class_species_dict

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
        numerical_triples, numerical_eval_triples = self.create_numerical_triples()
        df_num_eval = pd.DataFrame(numerical_eval_triples)
        df_num_eval.to_csv(f"{self.sub_ontology_path}/numerical_eval.tsv", sep="\t", header=False, index=False)
        with open(f"{self.sub_ontology_path}/node_value_dict.json", "w") as f:
            f.write(json.dumps(self.node_value_dict))
            f.close()
        role_triples = self.create_triples_for_roles_of_species()
        # chemical_class_triples = self.create_triples_with_subclass()
        cached_chemical_classes = [line.strip() for line in open(f"{self.full_dataset_dir}/chemical_classes.tsv").readlines()]
        chemical_class_triples = []
        for line in cached_chemical_classes:
            s, p, o = [e.strip() for e in line.split("\t")]
            chemical_class_triples.append((s, p, o))
        other_triples = role_triples + chemical_class_triples  # numerical_triples
        self.write_triples_to_tsv(other_triples=other_triples, numerical_triples=numerical_triples)
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
    my_analyzer = OntoSpeciesNewAnalyzer(sub_ontology="role_with_subclass_full_attributes_0.1_with_class")
    my_analyzer.run()
