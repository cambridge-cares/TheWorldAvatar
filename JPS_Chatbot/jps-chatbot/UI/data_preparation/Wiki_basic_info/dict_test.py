from os import listdir
from os.path import isfile, join
import json, time

with open('URI_SMILES_DICT') as f:
    dict = json.loads(f.read())
 
start_time = time.time() # time the process 

print(dict['C12=CCC=C1C=CC=C2'])
print(dict['C(C1C(C(C(C(O1)O)O)O)O)O']) # make sure glucose is in the dict (SMILES length = 24)
print(dict['CC'])
print(dict['C'])
print(dict['C1=CC=CC=CCC1']) 
print(dict['C1=CC=CC=C1']) 
 
print('it took', time.time() - start_time)
