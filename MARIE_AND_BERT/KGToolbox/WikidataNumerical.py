import json

import pandas as pd

from Marie.Util.location import DATA_DIR
import os

# TODO: go through the value_dictionary, get the hash - numerical value pairs
# we also need to know the properties

# e.g. [embedding, 55 , 232 , 1.5, NaN]
# [55, 66]


# find aromatic hydrocarbons with molar mass more than 6 g/mol

# 1. class query, attached to NEL, get list of entities under certain class
# 2. molar mass, find the list of value nodes
# 3. extract subset fulfilling numerical conditions

# experiment 1. bert based condition operator recognition, together with a numerical line

# find species similar to benzene ...
# find species similar to benzene in molar weight
# find species similar to benzene in vapour pressure
# find species weight more than benzene

# find species similar to benzene in molar weight
# numerical part of the species

# Use TransEA for numerical embedding
# for each triple with numerical value, create a mapping to its numerical value, ignore the unit for now

# numerical_hash_list = []
# numerical_hash_dict = {}
#
# with open(os.path.join(DATA_DIR, "CrossGraph/wikidata/wikidata_value_dict.json")) as f:
#     wikidata_value_dict = json.loads(f.read())
#     for node_hash, value in wikidata_value_dict.items():
#         datatype = value['type']
#         if datatype == "quantity":
#             numerical_hash_list.append(node_hash)
#             amount = float(value['value']['amount'])
#             numerical_hash_dict[node_hash] = amount
#
# with open(os.path.join(DATA_DIR, "CrossGraph/wikidata_single/wikidata_single_numerical_hash_list.json"), 'w') as f:
#     f.write(json.dumps(numerical_hash_list))
#     f.close()
#
# with open(os.path.join(DATA_DIR, "CrossGraph/wikidata_single/wikidata_single_numerical_hash_dict.json"), 'w') as f:
#     f.write(json.dumps(numerical_hash_dict))
#     f.close()

import matplotlib.pyplot as plt

# x = [1, 1, 2, 3, 3, 5, 7, 8, 9, 10,
#      10, 11, 11, 13, 13, 15, 16, 17, 18, 18,
#      18, 19, 20, 21, 21, 23, 24, 24, 25, 25,
#      25, 25, 26, 26, 26, 27, 27, 27, 27, 27,
#      29, 30, 30, 31, 33, 34, 34, 34, 35, 36,
#      36, 37, 37, 38, 38, 39, 40, 41, 41, 42,
#      43, 44, 45, 45, 46, 47, 48, 48, 49, 50,
#      51, 52, 53, 54, 55, 55, 56, 57, 58, 60,
#      61, 63, 64, 65, 66, 68, 70, 71, 72, 74,
#      75, 77, 81, 83, 84, 87, 89, 90, 90, 91
#      ]

dataset_path = "CrossGraph/wikidata_single"
dataset_name = "wikidata_single"
train_triplets_numerical = [line.split('\t') for line in
                            open(os.path.join(DATA_DIR, dataset_path,
                                              f'{dataset_name}_numerical-train.txt')).read().splitlines()]

triple_hashes = []
for s, p, o in train_triplets_numerical:
    triple_hashes.append(o.strip())

value_distribution = []

upper_limit = 20000
lower_limit = 0

filtered_numerical_hash_dict = {}
filtered_numerical_hash_list = []

filtered_triples = []
with open(os.path.join(DATA_DIR, "CrossGraph/wikidata_single/wikidata_single_numerical_hash_dict.json")) as f:
    numerical_dict = json.loads(f.read())
    for key, value in numerical_dict.items():
        if key in triple_hashes:
            if lower_limit < value < upper_limit:
                value = value
                value_distribution.append(value)
                filtered_numerical_hash_dict[key] = value
                filtered_numerical_hash_list.append(key)

for s, p, o in train_triplets_numerical:
    if o in filtered_numerical_hash_list:
        filtered_triples.append((s, p, o))

df_train = pd.DataFrame(filtered_triples)
df_train.to_csv(os.path.join(DATA_DIR, "CrossGraph/wikidata_numerical/wikidata_single_numerical-train.txt"), sep="\t", header=False, index=False)
df_test = df_train.sample(frac=0.2)
df_test.to_csv(os.path.join(DATA_DIR, "CrossGraph/wikidata_numerical/wikidata_single_numerical-test.txt"), sep="\t", header=False, index=False)

df_valid = df_train.sample(frac=0.2)
df_valid.to_csv(os.path.join(DATA_DIR, "CrossGraph/wikidata_numerical/wikidata_single_numerical-valid.txt"), sep="\t", header=False, index=False)

with open(os.path.join(DATA_DIR, "CrossGraph/wikidata_numerical/numerical_hash_dict.json"), 'w') as f:
    f.write(json.dumps(filtered_numerical_hash_dict))
    f.close()

with open(os.path.join(DATA_DIR, "CrossGraph/wikidata_numerical/wikidata_numerical_numerical_hash_list.json"),
          'w') as f:
    f.write(json.dumps(filtered_numerical_hash_list))
    f.close()

average = sum(value_distribution) / len(value_distribution)
print("number of data", len(value_distribution))
print(average)
print("max", max(value_distribution))
print("min", min(value_distribution))
