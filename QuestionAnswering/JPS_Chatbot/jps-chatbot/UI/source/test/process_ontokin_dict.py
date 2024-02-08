import json
import os
from pprint import pprint


def extract_species(filename, the_dict):
    temp = []
    for obj in the_dict:
        species = obj['name']
        temp.append(species)
    with open(filename, 'w') as f:
        f.write(json.dumps(temp))

ontokin_list = []
for filename in os.listdir('../python_tests/'):
    if filename.startswith('ontokin_dict'):
        with open(filename) as f:
            content = f.read()
            tmp = json.loads(content.replace(' ', ''))
            ontokin_list = ontokin_list + tmp

ontokin_list = sorted(list(set(ontokin_list)))
# print(ontokin_list)

ontocompchem_list = []
ontocompchem_dict = {}
for filename in os.listdir('../python_tests/'):
    if filename.startswith('ontocompchem_dict_'):
        with open(filename) as f:
            content = f.read()
            original_tmp = json.loads(content)
            tmp = json.loads(content.replace(' ', ''))
            # make the intent -> species dictionary and save it to ontocompchem_dict
            # Do the same thing to ontokin
            the_key = filename.replace('ontocompchem_dict_', '').lower().replace('gaussian_file', 'guassian_file')
            print('the key', the_key)
            ontocompchem_dict[the_key] = original_tmp
            ontocompchem_list = ontocompchem_list + tmp

ontocompchem_list = sorted(list(set(ontocompchem_list)))

super_list = sorted(list(set(ontokin_list + ontocompchem_list)))

# pprint(ontocompchem_dict)

# print(len(super_list))
# print(super_list)

# with open('ontocompchem_dict', 'w') as f:
#     f.write(json.dumps(ontocompchem_dict))

ontokin_list = []
ontokin_dict = {}
for filename in os.listdir('../python_tests/'):
    if filename.startswith('ontokin_dict_'):
        with open(filename) as f:
            content = f.read()
            original_tmp = json.loads(content)
            t_filename =  filename.replace('ontokin_dict_', '').lower().replace('relaxation_collision', 'rotational_relaxation_collision').replace('lennard_jones_well_depth', 'lennard_jones_well')
            ontokin_dict[t_filename] = original_tmp
            print('filename', t_filename)
            print('ontokin\n')
            print(original_tmp)

with open('predicate-species-mapping/ontokin_dict', 'w') as f:
    f.write(json.dumps(ontokin_dict))

ontokin_simple_intents = ['polarizability',
                          'dipole_moment',
                          'rotational_relaxation_collision',
                          'lennard_jones_well']

ontocompchem_simple_intents = ['symmetry_number',
                               'rotational_constants',
                               'vibration_frequency',
                               'guassian_file',
                               'spin_multiplicity',
                               'formal_charge',
                               'electronic_energy',
                               'geometry_type']