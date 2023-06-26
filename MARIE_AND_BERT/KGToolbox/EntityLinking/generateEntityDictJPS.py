ONTOSPECIES = 'D:\work\Marie\MARIE_AND_BERT\DATA\Dictionaries\ontospecies\\name_dict.json'
ONTOKIN = 'D:\work\Marie\MARIE_AND_BERT\DATA\Dictionaries\ontokin\\name_dict.json'
TBOX = 'D:\work\Marie\chem_data\semi-auto-matching\generate_tbox_namedict\\class_plus_use_namelist.json'
import json

def toolong(name):
    return True if len(name) >= 100  else False
def generate_entity_dict(dictpath, fname):
    entries = []
    with open(dictpath) as p:
        original_dict = json.load(p)
    for idx, name in enumerate(original_dict):
        #iri = original_dict[name]
        entry = {}
        #entry['iri'] = iri
        entry['idx'] = idx
        entry['entity'] = name
        entry['title'] = name
        entry['text'] = ""

        if not toolong(name):
            entries.append(entry)
    with open(fname, 'w') as outfile:
        for entry in entries:
            json.dump(entry, outfile)
            outfile.write('\n')

'''
Replace with respect name_dict locations
'''
generate_entity_dict(ONTOKIN, 'ontokin.jsonl')