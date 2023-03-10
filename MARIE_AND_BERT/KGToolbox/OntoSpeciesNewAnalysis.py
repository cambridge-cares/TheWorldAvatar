import json
import os
import random
from pprint import pprint
import sys
sys.path.append("..")

sys.path.append("../../..")
import pandas as pd
from SPARQLWrapper import SPARQLWrapper, JSON
from sklearn.model_selection import train_test_split

from KGToolbox.IntegratedTrainingFileCreator import IntegratedTrainingFileCreator
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
        self.ontology = "ontospecies_new"
        self.sub_ontology = sub_ontology
        self.class_stop_list = ["rdf-schema#Resource", "rdf-schema#Class"]
        self.file_creator = IntegratedTrainingFileCreator(sparql_namespace="copy_ontospecies_marie",
                                                          ontology=self.ontology,
                                                          sub_ontology=sub_ontology, same_frac=0.01, other_frac=0)
        self.query_blazegraph = self.file_creator.query_blazegraph
        self.species_role_dictionary, self.role_species_dictionary = self.get_roles_of_species()
        self.species_class_dict, self.class_species_dict = self.get_all_chemical_classes()
        self.numerical_attribute_list = ["hasLogP", "hasDensity", "hasBoilingPoint",
                                         "hasSolubility", "hasLogP", "hasLogS", "hasMolecularWeight",
                                         "hasMeltingPoint"]

        self.numerical_attribute_species_node_dict, self.node_value_dict = self.get_all_numerical_attributes()

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

    def filter_inference_triples(self, triples_for_inference, sample_num=100):
        """
        Given all the triples selected for inference, create a sample of sample_num
        This function also makes sure that for each species-hasRole-Use, the Use-hasSpecies-Species are
        also removed from the
        :param triples_for_inference:
        :param sample_num:
        :return:
        """
        # print("triples for inference", triples_for_inference)
        # 1. make a list of species_role pair
        species_role_pair = []
        for triple in triples_for_inference:
            s, p, o = triple
            # print(s, p, o)
            if p.strip() == "hasUse":
                pair_str = f"{s}_{o}"
                if pair_str not in species_role_pair:
                    species_role_pair.append(pair_str)

        selected_species_role_pair = random.sample(species_role_pair, sample_num)
        selected_triples_for_inference = []
        selected_triples_for_training = []
        for triple in triples_for_inference:
            s, p, o = triple
            # if p.strip() == "hasRole":
            #     pair_str = f"{s}_{o}"  # species_use
            # else:
            pair_str = f"{s}_{o}"  # species_use
            if pair_str in selected_species_role_pair:
                selected_triples_for_inference.append(triple)
            else:
                selected_triples_for_training.append(triple)
        return selected_triples_for_inference, selected_triples_for_training

        # random choose 100 pairs

    def write_triples_to_tsv(self, numerical_triples, other_triples, class_triples):
        # only remove triples with hasRole and hasSpecies
        # 1. sample 200 triples with hasRole and hasSpecies
        # 2. remove the 200 triples from the other_triples
        inference_rels = ["hasUse", "hasSpecies"]
        other_triples_for_inference = [triples for triples in other_triples if triples[1].strip() in inference_rels]
        # if you remove the hasRole between a species and a use, the hasSpecies
        print("other triples for inference", other_triples_for_inference)
        test_other_triples, train_other_triples = self.filter_inference_triples(other_triples_for_inference,
                                                                                sample_num=100)
        # train_other_triples = [triples for triples in other_triples if triples not in test_other_triples]
        df = pd.DataFrame(numerical_triples + other_triples)
        df.to_csv(os.path.join(self.sub_ontology_path, f"{self.sub_ontology}-train.txt"),
                  sep="\t", header=False, index=False)

        df_train = pd.DataFrame(numerical_triples + train_other_triples + class_triples)
        df_test = pd.DataFrame(test_other_triples)
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
        counter = 0
        for species in self.species_class_dict:
            counter += 1
            print(f"{counter} out of {len(self.species_class_dict)}")
            full_class_list = self.species_class_dict[species]
            for class_list in full_class_list:
                for _class in class_list:
                    if _class not in self.class_stop_list:
                        row = (species, "hasChemicalClass", _class)
                        all_triples.append(row)

        # for _class in self.class_species_dict:
        #     if _class not in self.class_stop_list:
        #         species_list = self.class_species_dict[_class]
        #         for species in species_list:
        #             row = (_class, "hasInstance", species)
        #             # TODO: temporarily remove hasInstance triples
        #             # all_triples.append(row)
        return all_triples

    def create_triples_for_roles_of_species(self):
        all_triples = []
        for species in self.species_role_dictionary:
            role_list = self.species_role_dictionary[species]
            for role in role_list:
                row = (species, "hasUse", role)
                all_triples.append(row)

        for role in self.role_species_dictionary:
            species_list = self.role_species_dictionary[role]
            for species in species_list:
                row = (role, "hasSpecies", species)
                all_triples.append(row)

        # print("triples for roles of species", all_triples)
        return all_triples

    def get_roles_of_species(self):
        species_role_dict = {}
        role_species_dict = {}
        GET_ROLES_OF_SPECIES = """
        SELECT DISTINCT  ?species   ?role
        WHERE {
            # ?species ?p ?role .
            ?species <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#hasUse> ?role .
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
        print("Getting chemical class tree")
        query = """
        SELECT DISTINCT ?class   
        WHERE { <""" + chemical_class + """> rdf:type  ?class . 
        } 
        """
        sub_classes = []
        rst = self.query_blazegraph(query)["results"]["bindings"]
        for binding in rst:
            if "OntoSpecies.owl#ChemicalClass" not in binding["class"]["value"]:
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
            # if chemical_classes not in self.class_stop_list:
            chemical_class_list.append(chemical_classes)
            # sub_classes = self.get_chemical_class_tree(chemical_classes)
            # print("sub classes", sub_classes)
            # x = input()
            # chemical_class_list.extend(sub_classes)
            # for c in sub_classes:
            #     print("getting subclass", c)
            #     chemical_class_list.extend(self.get_chemical_class_tree(c))
            for i, c in enumerate(chemical_class_list):
                chemical_class_list[i] = c.split("/")[-1]
            species_class_dict = self.append_to_dict(species_class_dict, species, chemical_class_list)
            class_species_dict = self.append_to_dict(class_species_dict, chemical_class_list, species)

        return species_class_dict, class_species_dict

    # def create_inference_candidate_dict(self, entity2idx, dictionary):
    #     """
    #     Make a dictionary mapping true tail to candidate entities in the form of indices
    #     1. create all uses list
    #     :return:
    #     """
    #     candidate_dict = {}
    #     role_list = list(dictionary.keys())
    #     role_list = list(set(role_list))
    #     role_list = [entity2idx[role] for role in role_list]
    #     for role_idx in role_list:
    #         candidate_dict[role_idx] = role_list
    #     return candidate_dict

    def run(self):
        numerical_triples, numerical_eval_triples = self.create_numerical_triples()
        df_num_eval = pd.DataFrame(numerical_eval_triples)
        df_num_eval.to_csv(f"{self.sub_ontology_path}/numerical_eval.tsv", sep="\t", header=False, index=False)
        with open(f"{self.sub_ontology_path}/node_value_dict.json", "w") as f:
            f.write(json.dumps(self.node_value_dict))
            f.close()
        role_triples = self.create_triples_for_roles_of_species()

        cached_chemical_classes_path = f"{self.full_dataset_dir}/chemical_classes.tsv"
        if os.path.exists(cached_chemical_classes_path):
            cached_chemical_classes = [line.strip() for line in open(cached_chemical_classes_path).readlines()]
            chemical_class_triples = []
            for line in cached_chemical_classes:
                s, p, o = [e.strip() for e in line.split("\t")]
                chemical_class_triples.append((s, p, o))
        else:
            chemical_class_triples = self.create_triples_with_subclass()

        other_triples = role_triples + chemical_class_triples  # numerical_triples
        self.write_triples_to_tsv(other_triples=other_triples, numerical_triples=numerical_triples, class_triples = chemical_class_triples)

        # =================== STANDARD PACKAGE FOR FILES NEEDED ========================================
        self.file_creator.create_supporting_files_for_embedding(
            inference_target_dictionary=self.role_species_dictionary, node_value_dict=self.node_value_dict)
        # ==============================================================================================


if __name__ == "__main__":
    my_analyzer = OntoSpeciesNewAnalyzer(sub_ontology="test_para")
    my_analyzer.run()
