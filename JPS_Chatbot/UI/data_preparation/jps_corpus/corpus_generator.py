# use sparql query to collect all the labels and comments
# query all classes and properties, seperate the words out

from rdflib import Graph

g = Graph()
g.parse("./OntoKin.owl.xml")

properties_query_result = g.query(
    """
       prefix owl: <http://www.w3.org/2002/07/owl#>   
       prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
       SELECT DISTINCT ?property 
       WHERE {
          ?property rdf:type owl:ObjectProperty .
       }""")

for p in properties_query_result:
    print(p['property'])