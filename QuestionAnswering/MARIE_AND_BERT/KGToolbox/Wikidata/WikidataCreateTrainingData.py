import json
import os
import pickle
import random

import pandas as pd
from wikidata.client import Client
from Marie.Util.location import DATA_DIR

dataset_dir = "CrossGraph/wikidata_numerical"
triple_path = os.path.join(DATA_DIR, dataset_dir, "wikidata_numerical-train.txt")
unique_p = []
triples = open(triple_path).readlines()

numerical_p = []

numerical_triples = open(os.path.join(DATA_DIR, dataset_dir, "wikidata_numerical-train.txt")).readlines()
for n_triple in numerical_triples:
    s, p, o = [e.strip() for e in n_triple.split("\t")]
    if p not in numerical_p:
        numerical_p.append(p)

client = Client()

i2r_file = open(os.path.join(DATA_DIR, dataset_dir, 'idx2relation.pkl'), 'rb')
idx2rel = pickle.load(i2r_file)

r2i_file = open(os.path.join(DATA_DIR, dataset_dir, 'relation2idx.pkl'), 'rb')
rel2idx = pickle.load(r2i_file)

# 	question	head	tail	rel	numerical_operator

operator_dict = {
    "smaller": ["under", "smaller", "beneath", "less than", "smaller than"],
    "larger": ["above", "larger", "over", "more than", "larger than", "bigger than"],
    "about": ["about", "close to", "near", "around", "similar to"],
    "none": ["###", "@@@", "&&&", "***", "%%%", "$$$"]
}

# 	question	head	tail	rel	numerical_operator


label_dict = json.loads(open(os.path.join(DATA_DIR, dataset_dir, "p_dict.txt")).read())
lines = open(os.path.join(DATA_DIR, dataset_dir, "p_labels.txt")).readlines()[1:]

all_rows = []
for line in lines:
    _, p, label = (line.strip().split("\t"))
    alias = [l for l in label_dict[p] if ")" not in l]
    rel = p
    head = 0
    tail = 0
    if len(alias) >= 5:
        # choose the first five element, must include label
        alias = alias[0:4] + [label]
    else:
        # if not, repeat the list until reach 5
        alias = alias + [label] * (5 - len(alias))

    print(len(alias))
    # for one_label in random.sample(alias, min(1, len(alias))):
    for one_label in alias:
        if p in numerical_p:
            for key, operator_list in operator_dict.items():
                numerical_operator = key
                for operator_label in operator_list:
                    q_1 = operator_label + " " + one_label
                    q_2 = one_label + " " + operator_label
                    row_1 = (q_1, 0, 0, rel, numerical_operator)
                    row_2 = (q_2, 0, 0, rel, numerical_operator)
                    all_rows += [row_1, row_2]
        else:
            numerical_operator = "none"
            operator_list = operator_dict[numerical_operator]
            for i in range(0, 4):
                for operator_label in operator_list:
                    # operator_label = random.choice(operator_list)
                    q_1 = operator_label + " " + one_label
                    q_2 = one_label + " " + operator_label
                    row_1 = (q_1, 0, 0, rel, numerical_operator)
                    row_2 = (q_2, 0, 0, rel, numerical_operator)
                    all_rows += [row_1, row_2]

df = pd.DataFrame(all_rows)
df.columns = ["question", "head", "tail", "rel", "numerical_operator"]
df.to_csv(os.path.join(DATA_DIR, dataset_dir, "score_model_training.tsv"), sep="\t")
for ting in df['rel'].value_counts():
    print(ting)
