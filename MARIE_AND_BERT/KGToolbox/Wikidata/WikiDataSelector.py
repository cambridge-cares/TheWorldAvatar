import os
from pprint import pprint

import pandas as pd

from Marie.Util.location import DATA_DIR
wikidata_triples = open(os.path.join(DATA_DIR, 'CrossGraph/wikidata/wikidata-train.txt')).readlines()

selected_lines = ""
number_dict = {}
for line in wikidata_triples:
    s, _, _ = line.split('\t')
    l = len(s)
    if l in number_dict:
        number_dict[l] = number_dict[l] + 1
    else:
        number_dict[l] = 1
    if l <= 7:
        selected_lines = selected_lines + line

ontology_name = "wikidata_cs"

with open(os.path.join(DATA_DIR, f'CrossGraph/{ontology_name}/{ontology_name}-train.txt'), 'w') as f:
    f.write(selected_lines)
    f.close()

df_train = pd.read_csv(os.path.join(DATA_DIR, 'CrossGraph/wikidata/wikidata-train.txt'), index_col=None, header=None, sep='\t')
print(df_train)

df_test = df_train.sample(frac=0.2)
df_valid = df_train.sample(frac=0.2)

df_test.to_csv(os.path.join(DATA_DIR, 'CrossGraph/wikidata/wikidata-test.txt'), index=False, header=False, sep='\t')
df_valid.to_csv(os.path.join(DATA_DIR, 'CrossGraph/wikidata/wikidata-valid.txt'), index=False, header=False, sep='\t')

pprint(number_dict)

