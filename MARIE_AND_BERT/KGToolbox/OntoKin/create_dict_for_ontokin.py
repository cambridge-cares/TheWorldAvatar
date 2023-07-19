# create name_list and name_dict
import json
import time
import os
import pandas as pd
from Marie.Util.location import DICTIONARY_DIR, DATA_DIR

dict_dir = "ontokin"
name_list = []
name_dict = {}

path = os.path.join(DATA_DIR, "CrossGraph/ontokin")
df = pd.read_csv(os.path.join(path, 'all_species.tsv'), sep='\t', index_col=0)
species = df['species'].values.tolist()
labels = df['label'].values.tolist()

for s, l in zip(species, labels):
    if '#' in s:
        short_species = s.split('#')[-1]
    else:
        short_species = s.split('/')[-1]
    keys = [l]
    name_list = name_list + keys
    for k in keys:
        if k not in name_dict:
            name_dict[k] = short_species

name_list = list(set(name_list))

with open(os.path.join(DICTIONARY_DIR, dict_dir, 'name_list.json'), 'w') as f:
    f.write(json.dumps(name_list))
    f.close()

with open(os.path.join(DICTIONARY_DIR, dict_dir, 'name_dict.json'), 'w') as f:
    f.write(json.dumps(name_dict))
    f.close()
