# the purpose of this test is to systematically test out the questions
# in the system.

# preferably, automate the test via the front end.
# there are several steps involved:
# TODO: 1. Collect all the questions we can answer now, take note of the SPARQL, make a dictionary.
# TODO: 2. Get all the species in the KG that provide answers to such questions.
# TODO: 3. Automatically generate the questions set for testing ...
# TODO: 4. Use selenium to automatically test out the front end ... maybe its not a good idea, lets try out

# TODO: go modify the SPARQL generator, make logs of the SPARQL queries generated.
import json
import urllib
import urllib.parse
import urllib.request
from pprint import pprint
from ONTOKIN_QUERY_DICT import query_list_ontokin
import time

RotationalRelaxationCollisionNumber = '''

PREFIX ontokin: <http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>
PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
SELECT DISTINCT ?Species ?label ?RotationalRelaxationCollisionNumber
{
  ?Species rdfs:label ?label .
  ?Species ontokin:hasTransportModel ?TransportModel .
  ?TransportModel rdf:type ontokin:TransportModel .
  ?TransportModel ontokin:hasRotationalRelaxationCollisionNumber  ?RotationalRelaxationCollisionNumber .
}
'''


def fire_query_ontokin(query):
    print('----------- firing the query to ontokin -------------')
    # print(query)
    url = "http://www.theworldavatar.com/OntoKinGUI/OntoKinEndpointProxy"
    values = {'queryString': query}
    data = urllib.parse.urlencode(values).encode('utf-8')
    req = urllib.request.Request(url, data)
    response = urllib.request.urlopen(req).read()
    return response


def fire_query_ontochemcomp(query):
    print('----------- firing the query to JPS ontochemcomp -------------')
    # print(query)
    # self.socketio.emit('coordinate_agent', 'Querying the OntoCompChem ontology in the JPS Knowledge Graph')

    # x = input()
    url = "http://www.theworldavatar.com/rdf4j-server/repositories/ontocompchem"
    values = {'query': query}
    data = urllib.parse.urlencode(values).encode('utf-8')
    # print(type(data))
    req = urllib.request.Request(url, data)
    response = urllib.request.urlopen(req).read()
    return response


#

# r = fire_query_ontokin(RotationalRelaxationCollisionNumber)
#
# with open('test_query_result', 'w') as f:
#     f.write(str(r))

def parse_test_query_result():
    with open('test_query_result') as f:
        result = f.read()
        pprint(json.loads(result))


# parse_test_query_result_ontokin()
# Make a dictionary, with question type names.

def write_ontokin_dictionary(key, result):
    with open('ontokin_dict_%s' % key, 'w') as f:
        f.write(json.dumps(result,indent=4))


for ontokin_query in query_list_ontokin:
    key = ontokin_query
    query = query_list_ontokin[key]
    print(query)
    r = fire_query_ontokin(query)
    print('the result is', r.decode('utf-8'))
    species = r.decode('utf-8')
    # species = [x for x in species if x != 'name' and (x != '')]
    # ontocompchem_result[key] = species
    write_ontokin_dictionary(key, species)
    time.sleep(60)


# TODO: the species are in different forms ... now, try something ...