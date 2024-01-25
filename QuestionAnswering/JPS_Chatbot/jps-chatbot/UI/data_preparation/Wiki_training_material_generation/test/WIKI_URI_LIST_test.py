import json

with open('../WIKI_URI_LIST') as f:
    uri_list = json.loads(f.read())


for uri in uri_list[:2000]:
    print(uri)
    print(len(uri))