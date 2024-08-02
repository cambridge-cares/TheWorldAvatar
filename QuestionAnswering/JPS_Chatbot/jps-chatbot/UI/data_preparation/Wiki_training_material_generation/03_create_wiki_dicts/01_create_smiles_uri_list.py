import os
import time
from os import listdir
from os.path import isfile, join
import json

from location import TRAINING_FILES_DIR
##############################################
#
# This script is written during the process of 
# automating the trainning of the Marie chatbot. 
# 
# The purpose of it is to filter species by the 
# length of their SMILES string, in order to 
# filter out the super rare species
#
# Both the SMILES-URI dictionary and the 
# list of all species in wikidata are produced here 
#
##############################################


# get the files in the directory, their filename should begin with SMILE_
mypath = 'E:/data/smiles'
onlyfiles = [f for f in listdir(mypath) if isfile(join(mypath, f)) and f.startswith('SMILE_')]

counter_all = 0
counter_valid = 0

list = []
dict = {}
smiles = []
uris = []

SMILES_URI_DICT = {}
URI_SMILES_DICT = {}

duplication_dict = {}

length_threshold = 500
# iterate through the files, each file contains a list of SMILE-URI pairs
for file in onlyfiles:
    with open('E:/data/smiles/' + file) as f:
        print('Processing' + file)
        obj = json.loads(f.read())
        for item in obj:
            counter_all = counter_all + 1
            URI = item['species']  # get the uri
            SMILES = item['SMILES']  # get the SMILES string
            if SMILES in SMILES_URI_DICT:  # this SMILES already exists
                SMILES_URI_DICT[SMILES].append(URI)
            else:  # create a new array to hold the result
                SMILES_URI_DICT[SMILES] = [URI]
            URI_SMILES_DICT[URI] = SMILES
            if len(SMILES) < length_threshold:
                uris.append(URI)
                counter_valid = counter_valid + 1
        f.close()

# 52000 out of 837000 when threshold is 20, 126939 out of 837000 when 25.
print(counter_valid, 'out of', counter_all)

# write to the dictionary file, key to be SMILES, value to be URI
with open(os.path.join(TRAINING_FILES_DIR, 'WIKI_URI_LIST'), 'w') as f:
    f.write(json.dumps(SMILES_URI_DICT))
    f.close()

with open(os.path.join(TRAINING_FILES_DIR, 'URI_SMILES_DICT'), 'w') as f:
    f.write(json.dumps(URI_SMILES_DICT))
    f.close()

# check duplication
s_t = time.time()
print(SMILES_URI_DICT['C12=CCC=C1C=CC=C2'])
print(SMILES_URI_DICT['C(C1C(C(C(C(O1)O)O)O)O)O'])  # make sure glucose is in the list (SMILES length = 24)
print(SMILES_URI_DICT['CC'])
print(SMILES_URI_DICT['C'])
print(SMILES_URI_DICT['O'])
print(URI_SMILES_DICT['http://www.wikidata.org/entity/Q37129'])
print(URI_SMILES_DICT['http://www.wikidata.org/entity/Q283'])
print(time.time() - s_t)
with open(os.path.join(TRAINING_FILES_DIR, 'WIKI_URI_LIST'), 'w') as f:
    f.write(json.dumps(sorted(uris, key=len)))
    f.close()

with open(os.path.join(TRAINING_FILES_DIR, 'URI_SMILES_DICT'), 'w') as f:
    f.write(json.dumps(URI_SMILES_DICT))
    f.close()

with open(os.path.join(TRAINING_FILES_DIR,'SMILES_URI_DICT'), 'w') as f:
    f.write(json.dumps(SMILES_URI_DICT))
    f.close()