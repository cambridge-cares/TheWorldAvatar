import json
import os
import random
import sys

sys.path.append("../..")

sys.path.append("../../../..")
import pandas as pd

from KGToolbox.Tools.IntegratedTrainingFileCreator import IntegratedTrainingFileCreator

from Marie.CandidateSelection.location import DATA_DIR


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
        self.full_dataset_dir = os.path.join(DATA_DIR, "CrossGraph", self.ontology)
        self.sub_ontology_path = os.path.join(DATA_DIR, "CrossGraph", self.ontology, self.sub_ontology)
        self.use_hierarchy_triples = json.loads(open(f"{self.full_dataset_dir}/use_hierarchy_triples.json").read())

        self.preferred_use_list = \
            list(set([x.strip() for x in open(f"{self.full_dataset_dir}/filtered_raw_use_list.txt").readlines()]))

        self.duplicate_class_iri_mapping = json.loads(open(f"{self.full_dataset_dir}/class_iri_mapping.json").read())
        self.duplicate_use_iri_mapping = json.loads(open(f"{self.full_dataset_dir}/use_iri_mapping.json").read())

        self.class_label_dict = json.loads(open(f"{self.full_dataset_dir}/class_label_dict.json").read())
        self.use_label_dict = json.loads(open(f"{self.full_dataset_dir}/use_label_dict.json").read())
        self.label_use_dict = {v: k for k, v in self.use_label_dict.items()}
        self.preferred_use_iri_list = [self.label_use_dict[use] for use in self.preferred_use_list]
        self.class_stop_list = ["rdf-schema#Resource", "rdf-schema#Class",
                                "ChemicalClass_d3600db9-5c10-4108-a3fd-2757a4ff7796",
                                "ChemicalClass_24909ff7-9b07-4eb7-89b0-e96dabaec8e9",
                                "ChemicalClass_29cbc219-4263-405d-8c33-a1c53f4b08d2",
                                "ChemicalClass_b1528f24-c942-408f-9d38-8ae6aacd17bd"]
        self.selected_species = []
        self.file_creator = IntegratedTrainingFileCreator(sparql_namespace="copy_ontospecies_pubchem",
                                                          ontology=self.ontology,
                                                          sub_ontology=sub_ontology, same_frac=1, other_frac=0.1)

        self.class_caching_dict_path = os.path.join(self.full_dataset_dir, "class_tree_dict.json")
        if os.path.exists(self.class_caching_dict_path):
            self.class_caching_dict = json.loads(open(self.class_caching_dict_path).read())
        else:
            self.class_caching_dict = {}
        self.sub_class_triples = self.create_subclass_triples()
        self.query_blazegraph = self.file_creator.query_blazegraph
        print("1. STARTING SPECIES ROLE QUERYING")
        self.species_role_dictionary, self.role_species_dictionary = self.get_roles_of_species()
        print("2. DONE SPECIES ROLE QUERYING")
        print("3. STARTING SPECIES CLASS QUERYING")
        # do a filtering, return list of species ...
        self.species_class_dict, self.class_species_dict = self.get_all_chemical_classes()
        with open(self.class_caching_dict_path, "w") as f:
            f.write(json.dumps(self.class_caching_dict))
            f.close()
        print("4. DONE SPECIES CLASS QUERYING")
        self.selected_species, self.selected_inference_species, self.role_count_dict = self.select_species_by_density()
        print("1. STARTING SPECIES ROLE QUERYING")
        self.species_role_dictionary, self.role_species_dictionary = self.get_roles_of_species()
        self.selected_role_list = list(set(self.role_species_dictionary.keys()))
        self.role_count_dict = dict(sorted(self.role_count_dict.items(), key=lambda k: k[1], reverse=True))
        self.selected_inference_role_list = [r for r, l in list(set(self.role_count_dict.items()))]  # if l > 1]
        print("SELECTED SPECIES NUMBER", len(self.selected_species))
        print("SELECTED SPECIES FOR INFERENCE NUMBER", len(self.selected_inference_species))
        print("2. DONE SPECIES ROLE QUERYING")
        print("3. STARTING SPECIES CLASS QUERYING")
        # do a filtering, return list of species ...
        self.species_class_dict, self.class_species_dict = self.get_all_chemical_classes()
        print("4. DONE SPECIES CLASS QUERYING")

        self.numerical_attribute_list = ["hasLogP", "hasDensity", "hasBoilingPoint",
                                         "hasSolubility", "hasLogP", "hasLogS", "hasMolecularWeight",
                                         "hasMeltingPoint"]

        self.numerical_attribute_species_node_dict, self.node_value_dict = self.get_all_numerical_attributes()

    def create_subclass_triples(self):
        all_triples = []
        for _class in self.class_caching_dict:
            parent_class_list = self.class_caching_dict[_class]
            for parent_class in parent_class_list:
                parent_class_iri = parent_class.split("/")[-1]
                _class_iri = _class.split("/")[-1]
                triple_forward = (_class_iri, "isSubClassOf", parent_class_iri)
                triple_backward = (parent_class_iri, "hasSubClass", _class_iri)
                all_triples += [triple_forward, triple_backward]

        return all_triples

    def select_species_by_density(self):
        role_count_dict = {}
        species_role_count_dict = {}
        species_class_count_dict = {}
        combined_count_dict = {}
        species_tmp = []
        for species in self.species_role_dictionary:
            roles = self.species_role_dictionary[species]
            if len(roles) > 0:
                species_role_count_dict[species] = len(roles)
                species_tmp.append(species)
            for role in roles:
                if role in role_count_dict:
                    role_count_dict[role] += 1
                else:
                    role_count_dict[role] = 1

        counter = 0
        for species in self.species_class_dict:
            counter += 1
            classes = self.species_class_dict[species]
            if len(classes) > 0:
                species_class_count_dict[species] = len(classes)
                species_tmp.append(species)

        counter = 0
        for species in species_tmp:
            counter += 1
            if (species in species_class_count_dict) and (species in species_role_count_dict):
                combined_count_dict[species] = species_role_count_dict[species]

        combined_count_dict = dict(sorted(combined_count_dict.items(), key=lambda k: k[1], reverse=True))
        candidate_species_list = list(set(combined_count_dict.keys()))
        selected_species = candidate_species_list
        # selected_species = random.sample(candidate_species_list, min(1000, len(candidate_species_list)))
        # selected_species = list(set(combined_count_dict.keys()))[0:1000]
        selected_inference_species = random.sample(selected_species, 500)
        # selected_inference_species = selected_species[0:100]
        return selected_species, selected_inference_species, role_count_dict

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
        # 1. make a list of species_role pair
        inference_relation_list = ["hasUse"]  # , "hasChemicalClass"]
        species_role_pair = []
        for triple in triples_for_inference:
            s, p, o = triple
            if p.strip() in inference_relation_list:
                pair_str = f"{s}_{o}"
                if pair_str not in species_role_pair:
                    if s in self.selected_inference_species and o in self.selected_inference_role_list:
                        species_role_pair.append(pair_str)

        print("species_role_pair", species_role_pair)
        selected_species_role_pair = random.sample(species_role_pair, min(sample_num, len(species_role_pair)))
        print("selected species role pair", selected_species_role_pair)
        selected_triples_for_inference = []
        selected_triples_for_training = []

        used_species = []
        for triple in triples_for_inference:
            s, p, o = triple

            # if p.strip() == "hasRole":
            #     pair_str = f"{s}_{o}"  # species_use
            # else:
            pair_str = f"{s}_{o}"  # species_use
            if pair_str in selected_species_role_pair:
                if s not in used_species:
                    selected_triples_for_inference.append(triple)
                    used_species.append(s)
                else:
                    selected_triples_for_training.append(triple)
            else:
                selected_triples_for_training.append(triple)
        print("selected_triples_for_inference", selected_triples_for_inference)
        return selected_triples_for_inference, selected_triples_for_training

        # random choose 100 pairs

    def write_triples_to_tsv(self, numerical_triples, other_triples, class_triples):
        # only remove triples with hasRole and hasSpecies
        # 1. sample 200 triples with hasRole and hasSpecies
        # 2. remove the 200 triples from the other_triples
        # if you remove the hasRole between a species and a use, the hasSpecies
        test_other_triples, train_other_triples = self.filter_inference_triples(other_triples,
                                                                                sample_num=100)

        # 1. train.txt contains all the information
        df_all = pd.DataFrame(numerical_triples + other_triples + class_triples + self.use_hierarchy_triples)
        df_all.to_csv(os.path.join(self.sub_ontology_path, f"{self.sub_ontology}-train.txt"),
                      sep="\t", header=False, index=False)
        # 2. train-2.txt contains all the triples for training
        df_train = pd.DataFrame(numerical_triples + train_other_triples + class_triples + self.use_hierarchy_triples)
        df_train.to_csv(os.path.join(self.sub_ontology_path, f"{self.sub_ontology}-train-2.txt"),
                        sep="\t", header=False, index=False)
        # 3. test.txt contains all the triples used for inference
        df_test = pd.DataFrame(test_other_triples)
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
                if species in self.selected_species:
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
                        if _class in self.duplicate_class_iri_mapping:
                            _class = self.duplicate_class_iri_mapping[_class]
                            print("replaced class", _class)
                        row = (species, "hasChemicalClass", _class)
                        if species in self.selected_species:
                            all_triples.append(row)

        return all_triples

    def create_triples_for_roles_of_species(self):
        all_triples = []
        for species in self.species_role_dictionary:
            role_list = self.species_role_dictionary[species]
            for role in role_list:
                row = (species, "hasUse", role)
                if species in self.selected_species:
                    all_triples.append(row)

        return all_triples

    def get_roles_of_species(self):
        role_stop_list = ["Use_18404cd2-07a6-4dba-a590-bcc8f175686f",
                          "Use_e75e7e8f-a8e6-4134-8c03-ade83163f693",
                          "Use_1bd7ba7f-946e-43d9-a25a-933214829b03",
                          "Use_536b6f0e-e4a0-4e15-b405-dea190ee6317",
                          "Use_01ccfce8-0764-473d-8a3f-eb89e8d8dde1"]
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
            if species in self.selected_species or len(self.selected_species) == 0:
                if role not in role_stop_list:
                    if role in self.duplicate_use_iri_mapping:
                        role = self.duplicate_use_iri_mapping[role]
                        print("role is replaced", role)
                    species_role_dict = self.append_to_dict(dictionary=species_role_dict, key=species, value=role)
                    role_species_dict = self.append_to_dict(dictionary=role_species_dict, key=role, value=species)

        return species_role_dict, role_species_dict

    def get_chemical_class_tree(self, chemical_class):

        if chemical_class in self.class_caching_dict:
            return self.class_caching_dict[chemical_class]
        else:

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
            self.class_caching_dict[chemical_class] = sub_classes
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
        counter = 0
        rst = self.query_blazegraph(GET_CHEMICAL_CLASSES)["results"]["bindings"]
        for binding in rst:
            counter += 1
            print(f"{counter} out of {len(rst)}")
            chemical_class_list = []
            species = binding["species"]["value"].split("/")[-1]
            chemical_classes = binding["types"]["value"]
            if species in self.selected_species or len(self.selected_species) == 0:
                if chemical_classes not in self.class_stop_list:
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
        self.write_triples_to_tsv(other_triples=other_triples, numerical_triples=numerical_triples,
                                  class_triples=chemical_class_triples)

        inference_target_dictionary = self.role_species_dictionary
        # =================== STANDARD PACKAGE FOR FILES NEEDED ========================================
        entity2idx = self.file_creator.create_supporting_files_for_embedding(
            inference_target_dictionary=inference_target_dictionary, node_value_dict=self.node_value_dict)
        # ==============================================================================================

        self.selected_inference_role_list = [role for role in self.selected_inference_role_list if role in entity2idx]
        with open(f"{self.sub_ontology_path}/inference_role_list.json", "w") as f:
            f.write(json.dumps(self.selected_inference_role_list))
            f.close()

        with open(f"{self.sub_ontology_path}/class_label_dict.json", "w") as f:
            f.write(json.dumps(self.class_label_dict))
            f.close()
        with open(f"{self.sub_ontology_path}/use_label_dict.json", "w") as f:
            f.write(json.dumps(self.use_label_dict))
            f.close()

        print("SELECTED SPECIES NUMBER", len(self.selected_species))
        print("SELECTED SPECIES FOR INFERENCE NUMBER", len(self.selected_inference_species))


if __name__ == "__main__":
    my_analyzer = OntoSpeciesNewAnalyzer(sub_ontology="base_full_no_pref_selected_role_limited_100")
    my_analyzer.run()
