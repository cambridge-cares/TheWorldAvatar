from rdflib.plugins.sparql.parser import parseQuery
from rdflib.plugins.sparql.algebra import translateQuery
from rdflib import URIRef, BNode
import rdflib.plugins.sparql.algebra as algebra

class AnalyseSparql:
  
    def __init__(self,sparql_query):
        self.query_object = translateQuery(parseQuery(sparql_query))
        self.classes = set()
        self.properties = set()
    
    def extract_classes_and_properties(self):
        """Analyzes a SPARQL query to extract classes and properties.

        Args:
            sparql_query (str): The SPARQL query to analyze.

        Returns:
            tuple: A tuple containing two sets:
                * classes (set): A set of URIRefs representing classes.
                * properties (set): A set of URIRefs representing properties.
        """ 

        self.traverse_query_tree(self.query_object.algebra)
        return self.classes, self.properties

    def traverse_query_tree(self,node):
        # Safely check for and iterate over triples
        if hasattr(node, 'triples') and node.triples is not None:
            for s, p, o in node.triples:
                if isinstance(s, URIRef):
                    self.classes.add(s)
                if isinstance(p, URIRef):
                    self.properties.add(p)
                if isinstance(o, URIRef) and o.startswith("http"):
                    self.classes.add(o)  # Heuristic for class as object

        # Recursively traverse child nodes
        for attr in ['p', 'p1', 'p2', 'expr']:  # Common child node attributes
            if hasattr(node, attr):
                child = getattr(node, attr)
                if isinstance(child, list):
                    for item in child:
                        self.traverse_query_tree(item)
                else:
                    self.traverse_query_tree(child)
# Example usage
sparql_query = """
PREFIX ex: <http://example.org/>
SELECT ?subject ?type ?property
WHERE {
  ?subject rdf:type ex:Class1 .
  ?subject ex:property1 ?value1 .
  OPTIONAL { ?subject ex:property2 ?value2 . }
  FILTER(?value1 > 10)
  { ?subject ex:property3 ?value3 . }
  UNION
  { ?subject ex:property4 ?value4 . }
  GRAPH ?g {
    ?s ?p ?o .
  }
  VALUES (?a ?b) {
    (1 2)
    (3 4)
  }
  BIND(?value1 + 1 AS ?newValue)
  {
    SELECT ?sub ?prop WHERE {
      ?sub ex:prop ?prop .
    }
  }
}
"""

# usage
if __name__ == "__main__":
    ana = AnalyseSparql(sparql_query)
    classes, properties = ana.extract_classes_and_properties()
  
    print("Classes:", classes)
    print("Properties:", properties)
