# TODO:
# question, question_type, head, tail
import json
import os
import random

import pandas as pd

from Marie.Util.location import DATA_DIR

dataset_dir = "wikidata_numerical"
dict_path = os.path.join(DATA_DIR, "Dictionaries", dataset_dir, "name_dict.json")
name_dict = json.loads(open(dict_path).read())

iri2labels = {}

for label, iri in name_dict.items():
    if iri in iri2labels:
        if label not in iri2labels[iri]:
            iri2labels[iri].append(label)
    else:
        iri2labels[iri] = [label]


class WikidataNumericalTestSetCreator:

    def __init__(self):
        triple_path = os.path.join(DATA_DIR, "../KGToolbox/CrossGraph", dataset_dir, "wikidata_numerical-train.txt")
        numerical_triple_path = os.path.join(DATA_DIR, "../KGToolbox/CrossGraph", dataset_dir,
                                             "wikidata_numerical_numerical-train.txt")
        self.value_dictionary_path = os.path.join(DATA_DIR, "../KGToolbox/CrossGraph", dataset_dir,
                                                  f"wikidata_numerical_value_dict.json")

        self.value_dictionary = json.loads(open(self.value_dictionary_path).read())
        p_label_path = os.path.join("p_labels.txt")
        self.selected_p = [l.strip().split("\t")[1] for l in open(p_label_path).readlines()[1:]]
        self.selected_p_labels = {l.strip().split("\t")[1]: [l.strip().split("\t")[2]] for l in
                                  open(p_label_path).readlines()[1:]}

        self.selected_p_dict = json.loads(open("p_dict.txt").read())
        self.triples = open(triple_path).readlines()
        self.numerical_triples = open(numerical_triple_path).readlines()

        self.operator_dict = {
            "smaller": ["under", "smaller", "beneath", "less than", "smaller than"],
            "larger": ["above", "larger", "over", "more than", "larger than", "bigger than"],
            "about": ["about", "close to", "near", "around", "similar to"]
        }

        self.normal_question_template = "what is the %s of %s"
        self.numerical_question_template = "find all species with %s %s %s"

        selected_p_path = os.path.join(DATA_DIR, "../KGToolbox/CrossGraph", dataset_dir,
                                       "selected_p.json")

        with open(selected_p_path, "w") as f:
            f.write(json.dumps(self.selected_p))
            f.close()

    def create_numerical_questions(self):
        # e.g. find all species with molecular weight more than 100 g/mol
        # 1. go through all the numerical triples
        all_numerical_heads = []
        p_value_dict = {}
        p_value_o_dict = {}
        all_ = []
        for numerical_triple in self.numerical_triples:
            s, p, o = [e.strip() for e in numerical_triple.split("\t")]
            if s not in all_numerical_heads:
                all_numerical_heads.append(s)

            n_value = self.value_dictionary[o]
            if p in p_value_dict:
                p_value_dict[p].append(n_value)

            else:
                p_value_dict[p] = [n_value]

        p_num_operator_list = []
        for numerical_triple in self.numerical_triples:
            s, p, o = [e.strip() for e in numerical_triple.split("\t")]
            if p in self.selected_p:

                for operator_type, operator_label_list in self.operator_dict.items():
                    random_numerical_value = round(
                        random.uniform(min(p_value_dict[p]), sum(p_value_dict[p]) / len(p_value_dict[p])), 2)
                    p_num_operator = (p, operator_type, random_numerical_value)
                    p_num_operator_list.append(p_num_operator)

        p_num_operator_tails_list = []
        max_len = 0

        for p, operator_type, random_value in p_num_operator_list:
            full_filled_tails = []
            full_filled_tails_candidates = []
            for numerical_triple in self.numerical_triples:
                s, p_2, o = [e.strip() for e in numerical_triple.split("\t")]
                if p == p_2:
                    o_value = self.value_dictionary[o]
                    if operator_type == "smaller":
                        if o_value < random_value:
                            full_filled_tails.append(o)
                    elif operator_type == "larger":
                        if o_value > random_value:
                            full_filled_tails.append(o)
                    elif operator_type == "about":
                        full_filled_tails_candidates.append((abs(o_value - random_value), o))

            if operator_type == "about":
                full_filled_tails_candidates.sort(key=lambda y: y[0])
                full_filled_tails = [tail[1] for tail in full_filled_tails_candidates
                [0: min(10, len(full_filled_tails_candidates))]]

            p_num_operator_tails_list.append((p, operator_type, random_value, full_filled_tails))

        for p, operator_type, random_value, full_filled_tails in p_num_operator_tails_list:
            p_labels = self.selected_p_dict[p]
            p_labels += self.selected_p_labels[p]
            operator_label_list = self.operator_dict[operator_type]
            selected_p_label = random.choice(p_labels)
            operator_label = random.choice(operator_label_list)
            question = self.numerical_question_template % (selected_p_label, operator_label, random_value)
            row = (question, None, p, full_filled_tails, "numerical", None, operator_type)
            all_.append(row)

        numerical_test_set_path = os.path.join(DATA_DIR, "../KGToolbox/CrossGraph", dataset_dir,
                                               "wikidata_numerical_test_set-numerical.json")
        with open(numerical_test_set_path, "w") as f:
            f.write(json.dumps(all_))
            f.close()
        print(len(all_numerical_heads))

    def create_normal_questions(self):
        all_ = []
        # e.g. what is the vapour pressure of methane
        selected_p = []


        for triple in self.triples:
            s, p, o = [e.strip() for e in triple.split("\t")]
            if p in self.selected_p:
                if s in iri2labels:
                    species_label = random.choice(iri2labels[s])

                    # get the label of this species ...
                    p_labels = self.selected_p_dict[p]
                    p_labels += self.selected_p_labels[p]
                    selected_p_label = random.choice(p_labels)
                    question = self.normal_question_template % (selected_p_label, species_label)
                    row = (question, s, p, o, "normal", species_label, None, None)
                    all_.append(row)

        normal_test_set_path = os.path.join(DATA_DIR, "../KGToolbox/CrossGraph", dataset_dir,
                                            "wikidata_numerical_test_set-normal.json")
        print(len(all_))
        with open(normal_test_set_path, "w") as f:
            f.write(json.dumps(all_))
            f.close()
        # df = pd.DataFrame(all_)
        # df.columns = ["question", "head", "rel", "tail", "type", "mention", "operator_type", "numerical_value"]
        # df.to_csv("wikidata_numerical_test_set-normal.tsv", sep="\t")


if __name__ == "__main__":
    my_creator = WikidataNumericalTestSetCreator()
    # my_creator.create_normal_questions()
