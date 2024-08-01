# use sparql query to collect all the labels and comments
# query all classes and properties, seperate the words out
import json

import nltk
from rdflib import Graph
import re

ontologies = ['ontokin', 'ontocompchem']
tmp = []


def make_query(ontology):
    g = Graph()
    g.parse("./%s.owl.xml" % ontology)

    # query object properties
    object_properties_query_result = g.query(
        """
           prefix owl: <http://www.w3.org/2002/07/owl#>   
           prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
           prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
           SELECT DISTINCT ?property ?label  
           WHERE {
              ?property rdf:type owl:ObjectProperty .
              OPTIONAL {
              ?property rdfs:label ?label . 

              }
           }""")

    # query data properties
    data_properties_query_result = g.query(
        """
           prefix owl: <http://www.w3.org/2002/07/owl#>   
           prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
           prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
           SELECT DISTINCT ?property ?label  
           WHERE {
              ?property rdf:type owl:DatatypeProperty .

              OPTIONAL {
                ?property rdfs:label ?label . 
                }

              }
        """)
    for p in object_properties_query_result:
        process_query_result(p)

    for p in data_properties_query_result:
        process_query_result(p)

    return tmp


def process_query_result(p):
    # print(p['property'])
    stopwords = ['units', 'page', 'has']
    # filter out those words in the properties

    if p['label']:
        # print(p['label'])
        label = ' '.join([l for l in p['label'].strip().split(' ') if l not in stopwords])
        tmp.append(label.lower())

    else: # label doesn't exist, use the URI
        p_uri = p['property']
        if '#' in p_uri:
            name = p_uri.split('#')[-1]
        elif '/' in p_uri:
            name = p_uri.split('/')[-1]
        else:
            name = ''



        name = name.replace('cotational', 'rotational') # a typo in ontocompchem
        # when there is no label for the property, use the URI as the label, as URIs are in camel cases, use regex
        # to split the uris into words
        splitted = [item.lower() for item in re.sub('([A-Z][a-z]+)', r' \1', re.sub('([A-Z]+)', r' \1', name)).split() if
             item.lower() not in stopwords]
        p_string = ' '.join(splitted)
        tmp.append(p_string)



# iterate through the ontologies in the list
for ontology in ontologies:
    tmp = []
    make_query(ontology)
    print(tmp)
    print(len(tmp))
    with open('%s_properties' % ontology, 'w') as f:
        f.write(json.dumps(tmp, indent=4))
        f.close()
