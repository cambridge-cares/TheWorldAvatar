import json
import math
import re
import time
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


def test_3000():
    q3 = '''
    PREFIX ontokin: <http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>
    PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    SELECT DISTINCT ?label ?POLARIZABILITY ?Unit
    {

             ?TransportModel ontokin:hasPolarizability ?POLARIZABILITY .
         ?Species ontokin:hasTransportModel ?TransportModel .
         ?Species rdfs:label ?label .
             ?Species rdfs:label "C2H2O2" .

              OPTIONAL{
                    ?TransportModel ontokin:hasPolarizabilityUnits ?Unit .
         }

    }  LIMIT 1
    '''
    p1 = "OH"
    p2 = "H"
    r1 = "O"
    r2 = "H2"

    # q2 = q2 % (p1, p1, p2, p2)
    products = [p1, p2]
    reactants = [r2]

    start_time = time.time()

#     url = " http://localhost:53001/marie/ldf/query?"
    url = " http://localhost:3000/query?"
    values = {"query": q2, "products": json.dumps(products), "reactants": json.dumps(reactants), "ontology": "ontokin"}
    full_url = url + urllib.parse.urlencode(values)

    print(full_url)

    req = urllib.request.Request(full_url)
    response = urllib.request.urlopen(req).read()
    print(response)
    end_time = time.time()
    print(round(end_time - start_time, 2), 'seconds')
    # response = urllib.request.urlopen(req).read()

    q_ontocompchem = '''
    PREFIX compchemkb: <https://como.cheng.cam.ac.uk/kb/compchem.owl#>
    PREFIX gc: <http://purl.org/gc/>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX ontocompchem:<http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#>
    SELECT DISTINCT ?name ?SpinMultiplicity    
    WHERE  {
       ?Molecule  ontocompchem:hasSpinMultiplicity ?SpinMultiplicity .
       ?GeometryOptimization    gc:hasMolecule    ?Molecule .
       ?g_calculation  gc:isCalculationOn    ?GeometryOptimization .

     { 
    	SELECT DISTINCT ?g_calculation ?name
    	WHERE {

    	   ?g_calculation ontocompchem:hasInitialization ?initialization .
    	   ?initialization gc:hasMoleculeProperty ?molecule_property .	
       	   ?molecule_property gc:hasName ?name .

    	   ?molecule_property gc:hasName "C8H14" .

    	} LIMIT 1 
     } 
    } LIMIT 1
    '''

    # start_time = time.time()
    # url = " http://kg.cmclinnovations.com:81/marie/ldf/ontocompchem/query?"
    # values = {"query": q_ontocompchem}
    # full_url = url + urllib.parse.urlencode(values)
    # req = urllib.request.Request(full_url)
    # response = urllib.request.urlopen(req).read()
    # print(response)
    # end_time = time.time()
    # print(round(end_time - start_time, 2), 'seconds')


# iterations = 1
# for i in range(iterations):
    # test_3000()
# query_pubchem = '''
# PREFIX sachem: <http://bioinfo.uochb.cas.cz/rdf/v1.0/sachem#>

# SELECT * WHERE {
# ?COMPOUND sachem:substructureSearch [
#     sachem:query "CC(=O)Oc1ccccc1C(O)=O" ].
# }
# LIMIT 1000
# '''

# url = 'https://pubchem.ncbi.nlm.nih.gov/rest/rdf/query?'
# values = {'query': query_pubchem}
# full_url = url + urllib.parse.urlencode(values)

# print(full_url)

# req = urllib.request.Request(full_url)
# response = urllib.request.urlopen(req).read()
# print(response)

if __name__=="__main__":
    test_3000()