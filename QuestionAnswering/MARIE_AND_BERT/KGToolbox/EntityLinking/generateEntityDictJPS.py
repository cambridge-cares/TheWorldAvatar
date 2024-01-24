'''
Convert entity name dictionaries to format required by EL training.
Examples be like:
ONTOSPECIES = './MARIE_AND_BERT/DATA/Dictionaries/ontospecies/name_dict.json'
ONTOKIN = './MARIE_AND_BERT/DATA/Dictionaries/ontokin/name_dict.json'
'''
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
if __name__ == "__main__":
    from argparse import ArgumentParser
    parser = ArgumentParser()
    parser.add_argument('--infile', type=str,help='path to orginal name dict')
    parser.add_argument('--outfile', type=str,default='ontokin.json',help='path to output entity jsonl')
    opts = parser.parse_args()
    opts = vars(opts)
    infile = opts['infile']
    outfile = opts['outfile']
    generate_entity_dict(infile, outfile)