# TODO:  get all the numerical attributes
# 1. Load the value_node, find the attributes attached to them
import json
import os

import pandas as pd

from Marie.Util.CommonTools.FileLoader import FileLoader
from Marie.Util.location import DATA_DIR


class TrainingSetCreator:

    def __init__(self):
        self.ontology = "ontospecies_new"
        self.sub_ontology = "base_full_no_pref_selected_role_limited_100"
        self.ontology_path = os.path.join(DATA_DIR, "CrossGraph", self.ontology)
        self.full_dataset_dir = os.path.join(self.ontology_path, self.sub_ontology)
        self.file_loader = FileLoader(full_dataset_dir=self.full_dataset_dir, dataset_name=self.sub_ontology)
        self.node_value_dict = self.file_loader.load_value_dict(dict_type="json", file_name="node_value_dict.json")
        self.all_triples = self.file_loader.load_all_triples()
        self.entity2idx, idx2entity, self.rel2idx, idx2rel = self.file_loader.load_index_files()
        self.all_unique_numerical_attributes, self.all_unique_non_numerical_relations = self.get_all_numerical_attributes()
        self.attribute_label_list = {"hasLogS": ["logs", "log s"],
                                     "hasLogP": ["logp", "octanol-water partition coefficient"],
                                     "hasSolubility": ["solubility", "solubility"],
                                     "hasMeltingPoint": ["melting point", "freezing point"],
                                     "hasMolecularWeight": ["molar mass", "molecular weight"],
                                     "hasBoilingPoint": ["boiling point", "evaporation point"],
                                     "hasDensity": ["density", "denseness"]
                                     }
        self.operator_dict = {
            "smaller": ["under", "smaller", "beneath", "less than", "smaller than"],
            "larger": ["above", "larger", "over", "more than", "larger than", "bigger than"],
            # "about": ["about", "close to", "near", "around", "similar to"],
            "none": ["###", "@@@", "&&&", "***", "%%%", "$$$"]
        }

    # question	head	tail	rel	numerical_operator
    def create_non_numerical_questions(self):
        all_non_numerical_rows = []
        inference_question_list = ["predict the possible uses", "can be the uses", "possible uses", "possible rows"]
        for question in inference_question_list:
            row = (question.lower(), 0, 0, "hasUse", "none")
            all_non_numerical_rows.append(row)

        return all_non_numerical_rows


    def create_numerical_comparison_questions(self):
        question_template = "%s %s"
        all_numerical_rows = []
        for attribute in self.all_unique_numerical_attributes:
            attribute_labels = self.attribute_label_list[attribute]
            for attribute_label in attribute_labels:
                for operator in self.operator_dict:
                    operator_labels = self.operator_dict[operator]
                    for operator_label in operator_labels:
                        question = question_template % (attribute_label, operator_label)
                        row = (question.lower(), 0, 0, attribute, operator)
                        all_numerical_rows.append(row)

        return all_numerical_rows


    def get_all_numerical_attributes(self):
        all_unique_numerical_attributes = []
        all_non_numerical_relations = []
        for triple in self.all_triples:
            s, p, o = triple.strip().split("\t")
            if o in self.node_value_dict:
                all_unique_numerical_attributes.append(p)
            else:
                all_non_numerical_relations.append(p)
        all_unique_numerical_attributes = list(set(all_unique_numerical_attributes))
        all_non_numerical_relations = list(set(all_non_numerical_relations))

        return all_unique_numerical_attributes, all_non_numerical_relations

    def run(self):
        numerical_rows = self.create_numerical_comparison_questions()
        non_numerical_rows = self.create_non_numerical_questions() * 10
        all_rows = numerical_rows + non_numerical_rows
        df = pd.DataFrame(all_rows)
        df.columns = ["question", "head", "tail", "rel", "numerical_operator"]
        df.to_csv(os.path.join(self.full_dataset_dir, "score_model_training.tsv"), sep="\t")


if __name__ == "__main__":
    my_creator = TrainingSetCreator()
    my_creator.run()
