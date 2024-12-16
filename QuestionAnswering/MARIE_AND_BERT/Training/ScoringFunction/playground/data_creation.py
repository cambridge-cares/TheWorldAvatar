# To create a question set where the questions with the mapping of their answer candidates
# e.g. What is the molecular weight of benzene,
# We currently assume that the head entity is known ...


import csv
import os
import random
from pprint import pprint
from Marie.Util.location import TRAINING_DIR

ent_label_path = os.path.join(TRAINING_DIR, r'ent_labels.tsv')
full_entity_list = [e.strip() for e in open(ent_label_path).readlines()]

# open pubchem csv and get all the relations

# extract all the names and formula and Inchi of all the species

# make 3 question template, special template for "molar weight and something"

file_path = os.path.join(TRAINING_DIR, 'pubchem.csv')
# how much does benenze weigh?

input_file = csv.DictReader(open(file_path).readlines()[0:50000])

species_name_mapping = {}
entity_list = []

for row in input_file:
    short_iupac_names = []
    formula_list = []
    inchi_standard_list = []

    iupac_name = row['iupac_name']
    formula = row['molecular_formula']
    inchi_standard = row['inchi_standard']
    CID = row['compound_id']
    entity_list.append(CID)
    if len(iupac_name) < 20:
        short_iupac_names.append(row['iupac_name'])
    formula_list.append(formula)

    name_list_for_one_species = short_iupac_names + formula_list
    name_list_for_one_species = [n for n in name_list_for_one_species if n.strip() != '']
    species_name_mapping[CID] = name_list_for_one_species

# stop_list = ['count_def_atom_stereo', 'count_undef_atom_setereo', 'count_def_bond_stereo', 'count_undef_bond_setereo']
stop_list = []
how_many_relations = []
normal_relations = []
headers = input_file.fieldnames

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
    t_1 = '''how many %s are in'''
    t_2 = '''what is the number of %s of'''
    full_relation_label = 'count_' + h_r.replace(' ', '_')
    full_relation_label_list.append(full_relation_label)
    for _CID in species_name_mapping:
        for name in species_name_mapping[_CID]:

            tail_entity = _CID + '_' + full_relation_label
            if tail_entity in full_entity_list:
                q_1 = (t_1 % (h_r), _CID, tail_entity.replace('\n', ''))
                q_2 = (t_2 % (h_r), _CID, tail_entity.replace('\n', ''))
                all_question = all_question + [q_1, q_2]
                entity_list.append(tail_entity)

for r in normal_relations:
    t_1 = '''what is the %s of'''
    t_2 = '''what is 's %s'''

    full_relation_label = r.replace(' ', '_')
    full_relation_label_list.append(full_relation_label)
    for _CID in species_name_mapping:
        for name in species_name_mapping[_CID]:
            tail_entity = _CID + '_' + full_relation_label
            if tail_entity in full_entity_list:
                entity_list.append(tail_entity)
                q_1 = (t_1 % (r), _CID, tail_entity.replace('\n', ''))
                q_2 = (t_2 % (r), _CID, tail_entity.replace('\n', ''))
                all_question = all_question + [q_1, q_2]

import csv

with open(os.path.join(TRAINING_DIR, 'question_set'), "w", newline='') as the_file:
    csv.register_dialect("custom", delimiter=",", skipinitialspace=True)
    writer = csv.writer(the_file, dialect="custom")
    for tup in all_question:
        writer.writerow(tup)

with open('entity_list', 'w', newline='\n') as f:
    for e in list(set(entity_list)):
        f.write(e + '\n')
