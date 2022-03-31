import glob
import requests
folder = "D:/workwork/ontoMatchData/Germany_subset"
url = "http://localhost:9999/blazegraph/namespace/powerplants/sparql"
#curl -D- -H 'Content-Type: text/turtle' --upload-file test.ttl -X POST 'http://blazegraph_instance/.../sparql?context-uri=https://named_graph_name'
values = {'context-uri': 'http://Germany'}
ctype = "application/rdf+xml"
headers = {'Content-type': ctype}

for file in glob.glob("D:/workwork/ontoMatchData/Germany_subset/*.owl"):
    print(file)
    with open(file, 'rb') as txt:
        payload = txt.read()
        files = {'upload_file': (open(file, 'rb'))}
        result = requests.post(url+"?context-uri=http://Germany", data=payload, headers=headers)
        print(result.text)