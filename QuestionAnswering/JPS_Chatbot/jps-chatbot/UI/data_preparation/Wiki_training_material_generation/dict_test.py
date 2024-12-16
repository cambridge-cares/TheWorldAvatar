from os import listdir
from os.path import isfile, join
import json, time

with open('SMILES_URI_DICT') as f:
    dict = json.loads(f.read())
 
start_time = time.time() # time the process 

with open('FULL_SMILES_LIST', 'w') as f2:
    f2.write(json.dumps(list(set(dict.keys()))))
    f2.close()

print(dict['C12=CCC=C1C=CC=C2'])
print(dict['C(C1C(C(C(C(O1)O)O)O)O)O']) # make sure glucose is in the dict (SMILES length = 24)
print(dict['CC'])
print(dict['C'])
print(dict['C1=CC=CC=CCC1']) 
print(dict['C1=CC=CC=C1']) 
 
print('it took', time.time() - start_time)
