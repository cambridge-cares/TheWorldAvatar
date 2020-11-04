import json

with open('wiki_dictionary_new') as f:
    wiki_dict = json.loads(f.read())