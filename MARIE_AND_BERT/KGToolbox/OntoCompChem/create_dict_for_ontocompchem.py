# create name_list and name_dict
import json
import time
import os
from pubchempy import Compound
from Marie.Util.location import DICTIONARY_DIR
from KGToolbox.Utils import query_blazegraph


dict_dir = "ontocompchem"

ONTOCOMPCHEM_GET_SPECIES_CID = """
SELECT DISTINCT ?species ?cid
WHERE {?species <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#pubChemCID> ?cid .}
"""
print("STARTED QUERY")
rst = query_blazegraph(query=ONTOCOMPCHEM_GET_SPECIES_CID, namespace="ontospecies")
print("FINISHED QUERY")
name_list = []
name_dict = {}

for r in rst['results']['bindings']:
    species = r['species']['value']
    pubchem_cid = r['cid']['value']
    cid = f"CID{pubchem_cid}"
    short_species = species.split('/')[-1]
    c = Compound.from_cid(pubchem_cid)
    formula = c.molecular_formula
    smiles = c.canonical_smiles
    iupac_name = c.iupac_name
    keys = [formula, smiles, iupac_name]
    name_list = name_list + keys
    for k in keys:
        if k not in name_dict:
            name_dict[k] = short_species
    time.sleep(5)
    print("CID:", cid)

name_list = list(set(name_list))

with open(os.path.join(DICTIONARY_DIR, dict_dir, 'name_list.json'), 'w') as f:
    f.write(json.dumps(name_list))
    f.close()

with open(os.path.join(DICTIONARY_DIR, dict_dir, 'name_dict.json'), 'w') as f:
    f.write(json.dumps(name_dict))
    f.close()
