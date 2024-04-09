import csv
import os
import random
from pprint import pprint

DATA_DIR = '../../Dataset'

# open pubchem csv and get all the relations

# extract all the names and formula and Inchi of all the species

# make 3 question template, special template for "molar weight and something"

file_path = os.path.join(DATA_DIR, 'pubchem.csv')
# how much does benenze weigh?


short_iupac_names = []
formula_list = []
inchi_standard_list = []
input_file = csv.DictReader(open(file_path).readlines()[0:500])
for row in input_file:
    iupac_name = row['iupac_name']
    formula = row['molecular_formula']
    inchi_standard = row['inchi_standard']

    if len(iupac_name) < 20:
        short_iupac_names.append(row['iupac_name'])
    formula_list.append(formula)
    inchi_standard_list.append(inchi_standard)

all_names_list = short_iupac_names + formula_list #  + inchi_standard_list
all_names_list = [n for n in all_names_list if n.strip() != '']

stop_list = ['count_def_atom_stereo', 'count_undef_atom_setereo', 'count_def_bond_stereo', 'count_undef_bond_setereo']
how_many_relations = []
normal_relations = []
headers = input_file.fieldnames

encode_map = {}

counter = 0
for head in headers:
    if head not in stop_list:



        if 'count_' in head:
            r = head.replace('count_', '').replace('_', ' ')
            how_many_relations.append(r)
        else:
            r = head.replace('_', ' ')
            normal_relations.append(r)

all_question = []


full_relation_label_list = []
for h_r in how_many_relations:
    t_1 = 'how many %s are in %s;%s'
    t_2 = 'what is the number of %s of %s;%s'
    full_relation_label = 'count_' + h_r.replace(' ', '_')
    full_relation_label_list.append(full_relation_label)
    for species in all_names_list:
        q_1 = t_1 % (h_r, species, full_relation_label)
        q_2 = t_2 % (h_r, species, full_relation_label)
        all_question = all_question + [q_1, q_2]


for r in normal_relations:
    t_1 = 'what is the %s of %s;%s'
    t_2 = "what is %s's %s;%s"
    full_relation_label = r.replace(' ', '_')
    full_relation_label_list.append(full_relation_label)
    for species in all_names_list:
        q_1 = t_1 % (r, species, full_relation_label)
        q_2 = t_2 % (species, r, full_relation_label)
        all_question = all_question + [q_1, q_2]

print(len(all_question))
train_set = random.sample(all_question, 3000)
all_question = [x for x in all_question if x not in train_set]
# valid_set = random.sample(all_question, 1000)
# all_question = [x for x in all_question if x not in train_set]
test_set = random.sample(all_question, 1000)

with open('input/pubchem/train.txt', 'w') as train_file:
    train_file.write('\n'.join(train_set).lower())
    train_file.close()

with open('input/pubchem/test.txt', 'w') as test_file:
    test_file.write('\n'.join(test_set).lower())
    test_file.close()


for f_r in full_relation_label_list:
    encode_map[f_r] = counter
    counter = counter + 1

print(encode_map)