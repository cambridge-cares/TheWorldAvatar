# TODO: read pubchem csv, retrieve
# TODO: do fuzzy comparison.
# TODO: return the CID (for pubchem)
import json
import os
import pickle
from pubchempy import *

import pandas as pd
from Marie.Util.location import TRAINING_DIR, DATA_DIR, DICTIONARY_DIR

dataset_name = "pubchem"
dataset_dir = os.path.join(DATA_DIR, f"CrossGraph/{dataset_name}")

def add_missing_cid():
    new_names = []
    new_dict = {}
    missing_cid_list = json.loads(open(os.path.join(dataset_dir, 'missing_cid_list.json')).read())
    for cid in missing_cid_list:
        for key in get_info_from_pubchem(cid=cid):
            if key is not None:
                new_dict[key] = 'CID' + str(cid)
                new_names = new_names + [key]

    return list(set(new_names)), new_dict


def get_info_from_pubchem(cid=None):
    c = Compound.from_cid(cid)
    formula = c.molecular_formula
    smiles = c.canonical_smiles
    iupac_name = c.iupac_name
    return formula, smiles, iupac_name

    # iupac_name
    # molecular_formula
    # cnonical smiles





STOP_INDEX = 10001

path = os.path.join(dataset_dir, 'pubchem.csv')
df_pubchem = pd.read_csv(path, sep=',')[0:STOP_INDEX]
# iupac_name, molecular_formula, canonical_smiles

# formula_dict = df_pubchem.set_index('molecular_formula').T.to_dict('compound_id')
CID_dict_name = dict([(k, v) for k, v in zip(df_pubchem['iupac_name'], df_pubchem['compound_id'])])
CID_dict_form = dict([(k, v) for k, v in zip(df_pubchem['molecular_formula'], df_pubchem['compound_id'])])
CID_dict_smil = dict([(k, v) for k, v in zip(df_pubchem['canonical_smiles'], df_pubchem['compound_id'])])

CID_dict = CID_dict_name
CID_dict.update(CID_dict_form)
CID_dict.update(CID_dict_smil)


name_list = [x for x in list(set(CID_dict.keys())) if str(x) != 'nan']


new_names, new_dict = add_missing_cid()
print(new_names, new_dict)
for k,v in new_dict.items():
    if k not in CID_dict:
        CID_dict[k] = v

name_list = name_list + new_names
name_list = list(set(name_list))


with open(os.path.join(DICTIONARY_DIR, dataset_name, 'name_list.json'), 'w') as f:
    f.write(json.dumps(name_list))
    f.close()


with open(os.path.join(DICTIONARY_DIR, dataset_name, 'cid_dict.json'), 'w') as f:
    f.write(json.dumps(CID_dict))
    f.close()

pubchem_value_dict = {}
for index, row in df_pubchem.iterrows():
    for col in df_pubchem.columns:
        cell_value = row[col]
        cell_label = row['compound_id'] + '_' + col
        pubchem_value_dict[cell_label] = cell_value

VALUE_DICT_PATH = os.path.join(DICTIONARY_DIR, dataset_name, 'pubchem_value_dict.pkl')
with open(VALUE_DICT_PATH, 'wb') as handle:
    pickle.dump(pubchem_value_dict, handle, protocol=pickle.HIGHEST_PROTOCOL)
