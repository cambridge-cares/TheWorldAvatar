from os import listdir
from os.path import isfile, join
import json


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
mypath = './smiles'
onlyfiles = [f for f in listdir(mypath) if isfile(join(mypath, f)) and f.startswith('SMILE_')]


counter_all = 0
counter_valid = 0

list = []
dict = {}
smiles = []
uris = []

SMILES_DICT = {}
 

duplication_dict = {} 

# iterate through the files, each file contains a list of SMILE-URI pairs
for file in onlyfiles:
    with open(file) as f:
        obj = json.loads(f.read())
        for item in obj:
            counter_all = counter_all + 1
            URI = item['species']  # get the uri
            SMILES = item['SMILES'] # get the SMILES string
            SMILES_DICT[SMILES] = URI            
            if len(SMILES) < 25:  # only keep the species with SMILES shorter than 25 
                smiles.append(SMILES)
                uris.append(URI)
                counter_valid = counter_valid + 1
                if URI in list:
                    dict[URI].append(SMILES)
                    duplication_dict[URI] = dict[URI]
                else:
                    dict[URI] = []
                list.append(URI)
        f.close()


# 52000 out of 837000 when threshold is 20, 126939 out of 837000 when 25. 
print(counter_valid,'out of',counter_all) 
              
# write to the dictionary file, key to be SMILES, value to be URI
with open('URI_SMILES_DICT', 'w') as f:
    f.write(json.dumps(SMILES_DICT))
    f.close()

# check duplication 
print(duplication_dict)
print('C12=CCC=C1C=CC=C2' in smiles)
print('C(C1C(C(C(C(O1)O)O)O)O)O' in smiles) # make sure glucose is in the list (SMILES length = 24)
print('CC' in smiles)
print('C' in smiles)
print('O' in smiles)
print('http://www.wikidata.org/entity/Q37129' in uris)
print('http://www.wikidata.org/entity/Q283' in uris)


with open('WIKI_URI_LIST', 'w') as f:
    f.write(json.dumps(sorted(uris)))
    f.close()
