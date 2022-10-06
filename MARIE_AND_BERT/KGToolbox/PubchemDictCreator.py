# TODO: read pubchem csv, retrieve
# TODO: do fuzzy comparison.
# TODO: return the CID (for pubchem)
import json
import os
import pickle

import pandas as pd
from Marie.Util.location import TRAINING_DIR

STOP_INDEX = 10001

path = os.path.join(TRAINING_DIR, 'pubchem.csv')
df_pubchem = pd.read_csv(path, sep=',')[0:STOP_INDEX]
# iupac_name, molecular_formula, canonical_smiles

# formula_dict = df_pubchem.set_index('molecular_formula').T.to_dict('compound_id')
CID_dict_name = dict([(k, v) for k, v in zip(df_pubchem['iupac_name'], df_pubchem['compound_id'])])
CID_dict_form = dict([(k, v) for k, v in zip(df_pubchem['molecular_formula'], df_pubchem['compound_id'])])
CID_dict_smil = dict([(k, v) for k, v in zip(df_pubchem['canonical_smiles'], df_pubchem['compound_id'])])

CID_dict = CID_dict_name
CID_dict.update(CID_dict_form)
CID_dict.update(CID_dict_smil)

# print(df_pubchem.columns)
# print(df_pubchem)
# print(CID_dict)
name_list = [x for x in list(set(CID_dict.keys())) if str(x) != 'nan']

with open(os.path.join(TRAINING_DIR, 'name_list.json'), 'w') as f:
    f.write(json.dumps(name_list))
    f.close()


with open(os.path.join(TRAINING_DIR, 'cid_dict.json'), 'w') as f:
    f.write(json.dumps(CID_dict))
    f.close()

pubchem_value_dict = {}
for index, row in df_pubchem.iterrows():
    for col in df_pubchem.columns:
        cell_value = row[col]
        cell_label = row['compound_id'] + '_' + col
        pubchem_value_dict[cell_label] = cell_value

VALUE_DICT_PATH = os.path.join(TRAINING_DIR, 'pubchem_value_dict.pkl')
with open(VALUE_DICT_PATH, 'wb') as handle:
    pickle.dump(pubchem_value_dict, handle, protocol=pickle.HIGHEST_PROTOCOL)