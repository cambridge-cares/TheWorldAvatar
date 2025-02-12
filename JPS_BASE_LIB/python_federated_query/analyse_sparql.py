from rdflib.plugins.sparql.parser import parseQuery
from rdflib.plugins.sparql.algebra import translateQuery
from rdflib import URIRef, BNode
import rdflib.plugins.sparql.algebra as algebra
import json

class AnalyseSparql:
  
    def __init__(self,sparql_query):
        self.query_object = translateQuery(parseQuery(sparql_query))
        self.class_index_file_path=""
        self.property_index_file_path=""
        self.cp_index_file_path=""
        self.classes = set()
        self.properties = set()
        self.class_index = {}
        self.property_index = {}
        self.cp_index = {}
    
    def set_index_location(self,index_dir):
      if(index_dir.strip()[-1]=="/"):
          index_dir=index_dir.strip()
      else:
          index_dir=index_dir.strip()+"/"
          
      self.class_index_file_path= index_dir+"cinv.indx"
      self.property_index_file_path=index_dir+"pinv.indx"
      self.cp_index_file_path=index_dir+"cpinv.indx"
      
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
    
    #Load index from file
    def load_indices(self):
        self.load_class_index()
        self.load_property_index()
        self.load_concept2property_index()
            
    def load_class_index(self):
        try:
            with open(self.class_index_file_path, 'r') as file:
                self.class_index = json.load(file)
            print(f"Class Index loaded from {self.class_index_file_path}")
        except FileNotFoundError:
            print(f"File '{self.class_index_file_path}' not found.")
        except json.JSONDecodeError:
            print(f"Error decoding JSON data from '{self.class_index_file_path}'.")

    def load_property_index(self):
        try:
            with open(self.property_index_file_path, 'r') as file:
                self.property_index = json.load(file)
            print(f"Property Index loaded from {self.property_index_file_path}")
        except FileNotFoundError:
            print(f"File '{self.property_index_file_path}' not found.")
        except json.JSONDecodeError:
            print(f"Error decoding JSON data from '{self.property_index_file_path}'.")
    
    def load_concept2property_index(self):
        try:
            # Open the file for reading
            with open(self.cp_index_file_path, 'r') as file:
                # Load the JSON data from the file into the index variable
                self.cp_index = json.load(file)
            print("Class-Property multilevel inverted index loaded successfully.")
        except FileNotFoundError:
            print(f"File '{self.cp_index_file_path}' not found.")
        except json.JSONDecodeError:
            print(f"Error decoding JSON data from '{self.cp_index_file_path}'.")
          
    def get_endpoints(self):
        endpoints = set()
        classes, properties = ana.extract_classes_and_properties()
        
        print("Analysing Classes:")
        for class_uriref in classes:
            class_uri=str(class_uriref)
            
            if class_uri in self.class_index:
                print("Found class alignment: " + class_uri)
                for endpoint in self.class_index[class_uri]:
                    endpoints.add(endpoint)
            else: print("Un-aligned class: "+class_uri)
            
        print("Analysing Properties:")        
        for property_uriref in properties:
            property_uri=str(property_uriref)
            if property_uri in self.property_index:
                print("Found property alignment:" + property_uri)
                for endpoint in self.property_index[property_uri]:
                    endpoints.add(endpoint)
            else: print("Un-aligned property: "+property_uri)
        
        print("The Final Endpoints: ")
        print(endpoints)
        
# Example usage
sparql_query = """
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX pt: <http://www.daml.org/2003/01/periodictable/PeriodicTable.owl#>
PREFIX OntoKin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?identifier ?atomicMass ?atomicMassUnits
WHERE {
    ?element1 rdf:type pt:Element .
    BIND(STRAFTER(STR(?element1), "#") AS ?identifier)
    ?element2 rdf:type OntoKin:Element .
    ?element2 rdfs:label ?identifier1 .
    ?element2 OntoKin:hasAtomicMass ?atomicMass .
    ?element2 OntoKin:hasAtomicMassUnits ?atomicMassUnits .
    FILTER(?identifier = ?identifier1)
}
"""

# usage
if __name__ == "__main__":
    index_location='C:/Users/printer_admin/Downloads/KGs/'
    
    ana = AnalyseSparql(sparql_query)
    ana.set_index_location(index_location)
    ana.load_indices()
    ana.get_endpoints()
        
    
