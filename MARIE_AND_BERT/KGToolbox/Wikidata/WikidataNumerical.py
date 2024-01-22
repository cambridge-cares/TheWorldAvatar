import json

import pandas as pd
from wikidata.entity import EntityId

from Marie.Util.location import DATA_DIR
import os
from wikidata.client import Client
client = Client()
# # TODO: go through the value_dictionary, get the hash - numerical value pairs
# # we also need to know the properties
#
# # e.g. [embedding, 55 , 232 , 1.5, NaN]
# # [55, 66]
#
#
# # find aromatic hydrocarbons with molar mass more than 6 g/mol
#
# # 1. class query, attached to NEL, get list of entities under certain class
# # 2. molar mass, find the list of value nodes
# # 3. extract subset fulfilling numerical conditions
#
# # experiment 1. bert based condition operator recognition, together with a numerical line
#
# # find species similar to benzene ...
# # find species similar to benzene in molar weight
# # find species similar to benzene in vapour pressure
# # find species weight more than benzene
#
# # find species similar to benzene in molar weight
# # numerical part of the species
#
# # Use TransEA for numerical embedding
# # for each triple with numerical value, create a mapping to its numerical value, ignore the unit for now
#
numerical_hash_list = []
numerical_hash_dict = {}
unique_unit_list = []
with open(os.path.join(DATA_DIR, "CrossGraph/wikidata/wikidata_value_dict.json")) as f:
    wikidata_value_dict = json.loads(f.read())
    for node_hash, value in wikidata_value_dict.items():
        datatype = value['type']
        if datatype == "quantity":
            unit = value['value']['unit']
            if unit not in unique_unit_list and "wikidata.org" in unit:
                unique_unit_list.append(unit)

new_value_dict = {}

unit_label_dict = {}
for unit_iri in unique_unit_list:
    unit = unit_iri.split('/')[-1]
    entity = client.get(EntityId(unit))
    print(entity.label)
    unit_label_dict[unit_iri] = str(entity.label)

with open(os.path.join(DATA_DIR, "CrossGraph/wikidata/wikidata_value_dict.json")) as f:
    wikidata_value_dict = json.loads(f.read())
    for node_hash, value in wikidata_value_dict.items():
        datatype = value['type']
        if datatype == "quantity":
            unit = value['value']['unit']
            if unit in unit_label_dict:
                unit_label = unit_label_dict[unit]
            else:
                unit_label = ""
            numerical_hash_list.append(node_hash)
            amount = float(value['value']['amount'])
            numerical_hash_dict[node_hash] = amount
            new_value_dict[node_hash] = str(amount) + ' ' + unit_label

with open(os.path.join(DATA_DIR, "CrossGraph/wikidata_numerical/wikidata_numerical_value_new.json"), 'w') as f:
    f.write(json.dumps(new_value_dict))
    f.close()
