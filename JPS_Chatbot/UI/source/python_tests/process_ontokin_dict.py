import json
import os


def extract_species(filename, the_dict):
    temp = []
    for obj in the_dict:
        species = obj['name']
        temp.append(species)
    with open(filename, 'w') as f:
        f.write(json.dumps(temp))

ontokin_list = []
for filename in os.listdir('./'):
    if filename.startswith('ontokin_dict'):
        with open(filename) as f:
            content = f.read()
            tmp = json.loads(content.replace(' ', ''))
            ontokin_list = ontokin_list + tmp

ontokin_list = sorted(list(set(ontokin_list)))
# print(ontokin_list)

ontocompchem_list = []
for filename in os.listdir('./'):
    if filename.startswith('ontocompchem_dict_'):
        with open(filename) as f:
            content = f.read()
            tmp = json.loads(content.replace(' ', ''))
            ontocompchem_list = ontocompchem_list + tmp

ontocompchem_list = sorted(list(set(ontocompchem_list)))

super_list = sorted(list(set(ontokin_list + ontocompchem_list)))

print(len(super_list))
print(super_list)
