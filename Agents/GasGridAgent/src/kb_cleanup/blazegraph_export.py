import requests

# URL to Blazegraph namespace
url = 'http://kg.cmclinnovations.com:81/blazegraph_geo/namespace/ts_backup/sparql'
payload = {'query': 'CONSTRUCT WHERE { hint:Query hint:analytic "true" . hint:Query hint:constructDistinctSPO "false" . ?s ?p ?o }'}
# Set the export format to be
#   rdf/xml: application/rdf+xml
#   or  ttl: application/x-turtle
headers = {
'Accept': 'application/rdf+xml'
}
# Run the http request
response = requests.request("POST", url, headers=headers, data = payload, files = [])

# Write results to file
with open("ts_backup_export.rdf", "w") as f:
    f.write(response.text)
