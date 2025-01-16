import json
import os
import random
from pprint import pprint

import pandas as pd

from KGToolbox.Tools.NLPWarehouse import COMPARISON_OPERATOR_LABEL_DICT, ONTOSPECIE_NEW_ATTRIBUTE_LABEL_DICT
from Marie.Util.CommonTools.FileLoader import FileLoader
from Marie.Util.location import DATA_DIR, DICTIONARY_DIR


class CreateOntoSpeciesNewEvaluationSet:

    def __init__(self):
        self.ontology = "ontospecies_new"
        self.sub_ontology = "base_full_no_pref_selected_role_limited_100"
        self.dataset_full_dir = os.path.join(DATA_DIR, "CrossGraph", self.ontology, self.sub_ontology)
        self.file_loader = FileLoader(full_dataset_dir=self.dataset_full_dir, dataset_name=self.sub_ontology)
        self.all_triples = self.file_loader.load_all_triples()
        self.entity2idx, self.idx2entity, self.rel2idx, self.idx2rel = self.file_loader.load_index_files()
        self.node_value_dict = self.file_loader.load_value_dict(dict_type="json", file_name="node_value_dict.json")
        self.numerical_attributes, self.numerical_triples = self.get_numerical_attributes()
        self.numerical_attributes_label_dict = ONTOSPECIE_NEW_ATTRIBUTE_LABEL_DICT
        self.question_template = "what is the %s of %s"
        dict_path = os.path.join(DICTIONARY_DIR, self.ontology, "name_dict.json")
        self.label_dict = json.loads(open(dict_path).read())
        self.species_to_label_dict = self.reverse_label_dict()
        self.species_list = list(set(self.species_to_label_dict.keys()))

        # get the species label

    def reverse_label_dict(self):
        species_label_dict = {}
        for label in self.label_dict["species"]:
            species_list = self.label_dict["species"][label]
            for species in species_list:
                species_label_dict[species] = label
        return species_label_dict

    def create_numerical_filtering_question(self):
        print("==========================")
        # attribute, operator, random number
        question_template = "find all species with %s %s %s"
        numerical_range_dict = {}
        numerical_value_dict = {}
        counter = 0
        for numerical_attribute in self.numerical_attributes:
            counter += 1
            print(f"{counter} out of {len(self.numerical_attributes)}")
            for triple in self.all_triples:
                s, p, o = triple.strip().split("\t")
                if p == numerical_attribute:
                    value = float(self.node_value_dict[o])
                    if 1e+5 >= value >= 1e-5:
                        if p in numerical_value_dict:
                            numerical_value_dict[p].append((o, value))
                        else:
                            numerical_value_dict[p] = [(o, value)]

        for p in numerical_value_dict:
            values = [v[1] for v in numerical_value_dict[p]]
            max_num = max(values)
            min_num = min(values)
            numerical_range_dict[p] = (min_num, max_num)

        all_numerical_filtering_question = []
        for numerical_attribute in self.numerical_attributes:
            min_n, max_n = numerical_range_dict[numerical_attribute]
            attribute_labels = self.numerical_attributes_label_dict[numerical_attribute]
            for attribute_label in attribute_labels:
                for operator in COMPARISON_OPERATOR_LABEL_DICT:
                    operator_labels = COMPARISON_OPERATOR_LABEL_DICT[operator]
                    for operator_label in operator_labels:
                        random_value = random.uniform(min_n, max_n)
                        question = question_template % (attribute_label, operator_label, random_value)
                        all_true_nodes = []
                        if operator == "smaller":
                            for node, value in numerical_value_dict[numerical_attribute]:
                                if value < random_value:
                                    all_true_nodes.append(node)
                        elif operator == "larger":
                            for node, value in numerical_value_dict[numerical_attribute]:
                                if value < random_value:
                                    all_true_nodes.append(node)
                        if len(all_true_nodes) > 0:
                            print("========================")
                            print("question:", question)
                            print("all_true_nodes:", all_true_nodes)
                            print("relation:", numerical_attribute)
                            question_object = {"question": question, "numerical_value": random_value, "all_true_tails":
                                               all_true_nodes, "relation": numerical_attribute, "operator": operator}
                            all_numerical_filtering_question.append(question_object)

        print("number of numerical filtering questions:", len(all_numerical_filtering_question))
        with open(os.path.join(self.dataset_full_dir, "numerical_test.json"), "w") as f:
            f.write(json.dumps(all_numerical_filtering_question))
            f.close()


    def get_numerical_attributes(self):
        unique_attributes = []
        numerical_triples = []
        for triple in self.all_triples:
            s, p, o = triple.strip().split("\t")
            if o in self.node_value_dict:
                unique_attributes.append(p)
                numerical_triples.append(triple)

        unique_attributes = list(set(unique_attributes))
        for x in unique_attributes:
            print(x)
        return unique_attributes, numerical_triples

    def create_inference_test_set(self):
        df_infer_path = os.path.join(self.dataset_full_dir, f"{self.sub_ontology}-test.txt")
        df_train_path = os.path.join(self.dataset_full_dir, f"{self.sub_ontology}-train.txt")
        df_inference = pd.read_csv(df_infer_path, sep="\t", header=None)
        df_train = pd.read_csv(df_train_path, sep="\t", header=None)
        inference_s_p_list = []
        # questions = ["what are the possible uses of %s", "what can %s be possible used for",
        #              "what are the rows of %s"]
        questions = ["predict the possible uses %s", "can be the uses %s", "possible uses %s", "possible rows %s"]


        #  ["question", "head", "domain", "answer", "mention", "relation"]
        other_true_tails = {}
        all_inference_question = {}
        for idx, row in df_inference.iterrows():
            s, p, o = row
            s_p_str = f"{s}_{p}"
            inference_s_p_list.append(s_p_str)
            for question_template in questions:
                species_label = self.species_to_label_dict[s]
                if len(species_label) < 15:
                    question = question_template % species_label
                    all_inference_question[s_p_str] = {"question": question, "mention": species_label, "true_tail": o}

        for idx, row in df_train.iterrows():
            s, p, o = row
            s_p_str = f"{s}_{p}"
            if s_p_str in all_inference_question:
                if s_p_str in other_true_tails:
                    other_true_tails[s_p_str].append(o)
                else:
                    other_true_tails[s_p_str] = [o]

        all_inference_question_list = []
        for s_p_str in other_true_tails:
            all_inference_question[s_p_str]["other_tails"] = other_true_tails[s_p_str]
            all_inference_question_list.append(all_inference_question[s_p_str])

        with open(os.path.join(self.dataset_full_dir, "inference_test.json"), "w") as f:
            f.write(json.dumps(all_inference_question_list))
            f.close()

    # df.columns = ["question", "head", "domain", "answer", "mention", "relation"]
    def create_numerical_test(self):
        normal_numerical_question = []
        for triple in self.all_triples:
            s, p, o = triple.strip().split("\t")
            if o in self.node_value_dict:
                attribute_labels = self.numerical_attributes_label_dict[p]
                for attribute_label in attribute_labels:
                    if s in self.species_to_label_dict:
                        species_label = self.species_to_label_dict[s]
                        if len(species_label) < 15:
                            question = self.question_template % (attribute_label, species_label)
                            head = s
                            domain = "ontospecies_new"
                            answer = o
                            mention = species_label
                            relation = self.rel2idx[p]
                            row = (question, head, domain, answer, mention, relation)
                            normal_numerical_question.append(row)
        df_normal_numerical = pd.DataFrame(normal_numerical_question)
        df_normal_numerical.columns = ["question", "head", "domain", "answer", "mention", "relation"]
        df_normal_numerical = df_normal_numerical.sample(n=100)
        df_normal_numerical.to_csv(os.path.join(self.dataset_full_dir, f"{self.ontology}_test_normal_numerical.tsv"),
                                   sep="\t")

    def run(self):
        self.create_numerical_test()
        self.create_inference_test_set()
        self.create_numerical_filtering_question()
        # self.get_numerical_attributes()
        # self.reverse_label_dict()


if __name__ == "__main__":
    my_creator = CreateOntoSpeciesNewEvaluationSet()
    my_creator.run()
