import json
import os
from rapidfuzz import process, fuzz
from location import TRAINING_FILES_DIR

dict_path = os.path.join(TRAINING_FILES_DIR, 'ONTOSPECIES_URI_DICT')
keys_path = os.path.join(TRAINING_FILES_DIR, 'ONTOSPECIES_KEYS')

with open(dict_path) as f:
    ontospecies_dict = json.loads(f.read())

with open(keys_path) as f:
    ontospecies_keys = json.loads(f.read())


def find_nearest_match(species, KEYS):
    species = species.strip()
    rst = process.extractOne(species, KEYS, scorer=fuzz.ratio)
    key = rst[0]
    score = rst[1]
    return key, score


def find_IRI(_key):
    return ontospecies_dict[_key]


to_be_tested = ['CO2', 'CH2=C', 'ch2=c', 'C10H15N', 'InChI=1/C10H15N/c1-3-8-6-5-7-9(4-2)10(8)11/h5-7H,3-4,11H2,1-2H3']
for _species in to_be_tested:
    _key, _score = find_nearest_match(_species, ontospecies_keys)
    print(_key, _score)
    print(find_IRI(_key))
    print('=====================')
