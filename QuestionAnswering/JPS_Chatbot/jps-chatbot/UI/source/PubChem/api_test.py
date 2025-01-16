import requests

test_get_attributes = 'https://pubchem.ncbi.nlm.nih.gov/rest/rdf/query?graph=compound&subj=compound:CID129626657&pred' \
                      '=sio:has-attribute&format=json'
# The above http request is equivalent to the following SPARQL query
# SELECT ?attributes
# WHERE {
#     compound:CID129626657 sio:has-attribute ?attributes
# }

test_get_value = 'https://pubchem.ncbi.nlm.nih.gov/rest/rdf/query?graph=descriptors&subject=descriptor:CID60823_Molecular_Weight&predicate=sio:has-value'

test_protein_example = 'https://pubchem.ncbi.nlm.nih.gov/rest/rdf/query?graph=protein&predicate=vocab:hasSimilarProtein&subject=protein:ACCP05979'

test_sparql_endpoint = 'https://idsm.elixir-czech.cz/sparql/endpoint/idsm?query=SELECT%20*%20WHERE%0A%7B%0A%20%20%3FS%20%3FP%20%3FO.%0A%7D%0ALIMIT%2010'

headers = {"Accept": "application/sparql-results+json",
           "Referrer Policy": "strict-origin-when-cross-origin"}

# print(test_sparql_endpoint)
# r = requests.get(test_sparql_endpoint, headers)
# print(r.text)

# test the post thing from I
import requests
import json
from urllib import parse

query = '''
PREFIX sachem: <http://bioinfo.uochb.cas.cz/rdf/v1.0/sachem#>

SELECT * WHERE {
?COMPOUND sachem:substructureSearch [
    sachem:query "CC(=O)Oc1ccccc1C(O)=O" ].
}
LIMIT 10
'''

query2 = '''
select (count(?sub) as ?subcnt) ?protein
from <http://rdf.ncbi.nlm.nih.gov/pubchem/substance>
from <http://rdf.ncbi.nlm.nih.gov/pubchem/measuregroup>
from <http://rdf.ncbi.nlm.nih.gov/pubchem/endpoint>
from <http://rdf.ncbi.nlm.nih.gov/pubchem/protein>
where {
  ?sub obo:RO_0000056 ?mg .
  ?mg obo:RO_0000057 ?protein .
  ?protein rdf:type bp:Protein .
  ?mg obo:OBI_0000299 ?ep .
  ?ep rdf:type bao:BAO_0000190 ; obo:IAO_0000136 ?sub ; sio:has-value ?value .
}
group by ?protein
order by ?subcnt
'''

url = 'https://idsm.elixir-czech.cz/sparql/endpoint/idsm'
body = {'query': query2}
headers = {'Content-Type': 'application/x-www-form-urlencoded',
           'Accept': 'application/sparql-results+json,*/*;q=0.9'}

data = parse.urlencode(body)

r = requests.post(url, data=data, headers=headers)
print(r.text)
