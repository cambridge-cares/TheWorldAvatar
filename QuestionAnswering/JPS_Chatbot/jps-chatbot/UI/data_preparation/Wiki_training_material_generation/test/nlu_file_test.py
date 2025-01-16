import json
import re
from pprint import pprint

expected_attributes = ['chemical formula', 'weight', 'molar mass',
                       'standard enthalpy of formation', 'geometry',
                       'heat capacity', 'upper flammable limit', 'lower flammable limit',
                       'ionization energy', 'standard molar entropy',
                       'dynamic viscosity', 'solubility', 'conjugate base', 'electric dipole moment',
                       'pKa', 'chemical structure']
expected_species = ['methane', 'water', 'h2o', 'ch4', 'cc', 'benzene','co', 'co2', 'carbon monoxide']
expected_classes = ['']


def find_classes(data):
    re_class = r'\[[a-z]+ *[a-z]* *[a-z]*\]\(class\)'
    rst = re.findall(re_class, data)
    class_list = set([c.replace('(class)', '').replace('[', '').replace(']', '') for c in rst])
    # pprint(class_list)
    # print(len(class_list))


with open('./data/nlu.md', encoding='utf-8') as f:
    training_data = f.read()
    f.close()

find_classes(training_data)

for ea in expected_attributes:
    ea = '[%s](attribute)' % ea.strip().lower()
    if ea not in training_data:
        print('Missing attribute', ea)

for es in expected_species:
    es = '[%s](species)' % es.strip().lower()
    if es not in training_data:
        print('Missing species', es)
