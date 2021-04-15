import json
import math
import re
import time
import urllib.parse
import urllib.request
import hashlib

q2 = '''
PREFIX rdf:      <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs:     <http://www.w3.org/2000/01/rdf-schema#>
PREFIX ontokin:  <http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>
PREFIX reaction: <http://www.theworldavatar.com/ontology/ontocape/material/substance/reaction_mechanism.owl#>
SELECT  DISTINCT  ?reaction ?Equation  
WHERE  {	
    ?reaction ontokin:hasEquation ?Equation .
} 

'''

p1 = "OH"
p2 = "H"
r1 = "O"
r2 = "H2"

products = [p1, p2]
reactants = [r2]


def make_query(_url, _values):
    start_time = time.time()
    parameter_hash = hashlib.md5(json.dumps(_values).encode('utf-8')).hexdigest()
    _values['hash'] = parameter_hash
    print(parameter_hash)
    full_url = _url + urllib.parse.urlencode(_values)
    req = urllib.request.Request(full_url)
    response = json.loads(urllib.request.urlopen(req).read().decode('utf-8'))

    print(time.time() - start_time, 'seconds')
    print(len(response['result']))


url = "http://localhost:3002/stream/query?"
values = {"query": q2, "products": json.dumps(products), "reactants": json.dumps(reactants)}
make_query(url, values)

for i in range(10):
    time.sleep(0.1)
    url = "http://localhost:3002/stream/loadmore?"
    values = {"query": q2, "products": json.dumps(products), "reactants": json.dumps(reactants)}
    make_query(url, values)

products = [p1]
reactants = [r2]

url = "http://localhost:3002/stream/query?"
values = {"query": q2, "products": json.dumps(products), "reactants": json.dumps(reactants)}
make_query(url, values)

for i in range(10):
    time.sleep(0.1)
    url = "http://localhost:3002/stream/loadmore?"
    values = {"query": q2, "products": json.dumps(products), "reactants": json.dumps(reactants)}
    make_query(url, values)
