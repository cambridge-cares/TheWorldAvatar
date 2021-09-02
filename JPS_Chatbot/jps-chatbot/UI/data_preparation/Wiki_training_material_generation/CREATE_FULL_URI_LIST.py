import os
from os import listdir
from os.path import isfile, join
import json

mypath = 'E:\data\smiles'
onlyfiles = [f for f in listdir(mypath) if isfile(join(mypath, f)) and f.startswith('SMILE_')]
counter_all = 0
file_counter = 0
FULL_URI_LIST = []

with open('FULL_URI_LIST', 'w') as f:
    f.write('')
    f.close()

for file in onlyfiles:
    file_counter = file_counter + 1
    print(file_counter, 'out of', len(onlyfiles))
    with open(os.path.join(mypath, file)) as f:
        obj = json.loads(f.read())
        for item in obj:
            counter_all = counter_all + 1
            URI = item['species']  # get the uri
            with open('FULL_URI_LIST', 'a') as f2:
                f2.write(URI)
                f2.write('\n')
                f2.close()
        f.close()


with open('FULL_URI_LIST',encoding='utf-8') as f:
    FULL_URI_LIST = f.readlines()
    FULL_URI_LIST = list(set(FULL_URI_LIST))
    f.close()

with open('FULL_URI_LIST', 'w') as f:
    f.write(json.dumps(FULL_URI_LIST, indent=4))
    f.close()
