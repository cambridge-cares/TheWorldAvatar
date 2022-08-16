import glob
import requests
folder = "D:\work\ontomatch-data\Germany_subset"
url = "http://www.theworldavatar.com/blazegraph/namespace/powerplants/sparql"
#curl -D- -H 'Content-Type: text/turtle' --upload-file test.ttl -X POST 'http://blazegraph_instance/.../sparql?context-uri=https://named_graph_name'
values = {'context-uri': 'http://kwl'}
ctype = "application/x-turtle"#"application/rdf+xml"
headers = {'Content-type': ctype}
kwlfile = "D:\work\ontomatch-data\kwl_geo.ttl"

#for file in glob.glob("D:\work\ontomatch-data\Germany_subset/*.owl"):
#    print(file)
with open(kwlfile, 'rb') as txt:
    payload = txt.read()
    result = requests.post(url+"?context-uri=http://kwl", data=payload, headers=headers)
    print(result.text)