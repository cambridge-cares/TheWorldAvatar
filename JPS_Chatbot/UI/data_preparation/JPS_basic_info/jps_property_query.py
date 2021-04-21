# use sparql query to collect all the labels and comments
# query all classes and properties, seperate the words out

from rdflib import Graph
import re

g = Graph()
g.parse("./OntoKin.owl.xml")

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
    """

)
def process_query_result(p):
    # print(p['property'])
    
    stopwords = ['units', 'page']
    
    if p['label']:
        print(p['label'])
    else:
        p_uri = p['property']
        if '#' in p_uri:
            name = p_uri.split('#')[-1]
        elif '/' in p_uri:
            name = p_uri.split('/')[-1]
              
        splitted = set([item.lower() for item in re.sub('([A-Z][a-z]+)', r' \1', re.sub('([A-Z]+)', r' \1', name)).split() if item.lower() not in stopwords])
        str = ' '.join(splitted)
        print(str)

for p in object_properties_query_result:
    process_query_result(p)
    
for p in data_properties_query_result:
    process_query_result(p)

