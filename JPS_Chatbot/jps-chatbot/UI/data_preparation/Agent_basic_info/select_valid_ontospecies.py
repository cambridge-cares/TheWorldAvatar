import json
dictionary = json.loads(open('../files/ONTOSPECIES_URI_DICT').read())
keys = dictionary.keys()
uris = open('../files/VALID_ONTOSPECIES_URI').readlines()

for u in uris:
    for k,v in zip(dictionary.keys(), dictionary.values()):
        print(v)
        if u in v:
            print(k)