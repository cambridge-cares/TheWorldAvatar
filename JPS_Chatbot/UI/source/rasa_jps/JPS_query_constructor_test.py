import json
import re
import urllib.parse
import urllib.request

from urllib.parse import urlencode

# try:
#     from __main__ import socketio
#
#     print('Importing socketIO from main in interpretation')
# except ImportError:
#     from run import socketio

print('----------- firing the query to LDF -------------')
query = '''
PREFIX ontokin: <http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>
PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
SELECT DISTINCT ?label ?LENNARDJONESWELLDEPTH ?Unit
{

         ?TransportModel ontokin:hasLennardJonesWellDepth ?LENNARDJONESWELLDEPTH .
     ?Species ontokin:hasTransportModel ?TransportModel .
     ?Species rdfs:label ?label .
         ?Species rdfs:label "C2H2O2" .

          OPTIONAL{
                ?TransportModel ontokin:hasLennardJonesWellDepth ?Unit .
     }

}  LIMIT 1
 
'''

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

#q2 = q2 % (p1, p1, p2, p2)
products = [p1, p2]
reactants = [ r2]

url = "http://localhost:3000/query?"
values = {"query": q2, "products": json.dumps(products), "reactants": json.dumps(reactants)}
full_url = url + urllib.parse.urlencode(values)
req = urllib.request.Request(full_url)
response = urllib.request.urlopen(req).read()

# response = urllib.request.urlopen(req).read()
