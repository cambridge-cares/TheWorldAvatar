import json

with open('../URI_SMILES_DICT') as f:
    URI_SMILES_DICT = json.loads(f.read())

with open('../SMILES_URI_DICT') as f:
    SMILES_URI_DICT = json.loads(f.read())

with open('../WIKI_URI_LIST') as f:
    WIKI_URI_LIST = json.loads(f.read())

from os import listdir
from os.path import isfile, join
mypath = '../instance_info'
onlyfiles = [f for f in listdir(mypath) if isfile(join(mypath, f))]

print(len(URI_SMILES_DICT))
print(len(SMILES_URI_DICT))
print(len(WIKI_URI_LIST))
print(len(onlyfiles))