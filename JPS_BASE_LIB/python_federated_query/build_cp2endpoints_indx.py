from rdflib import Graph, Namespace
from rdflib.plugins.stores.sparqlstore import SPARQLStore
import os
import json
import requests

class BuildKGIndex:
  
  def __init__(self):
    self.triplecount=0
    self.class_index = {}
    self.cp_index = {}
    self.graph = Graph()
    
        
  def start_processing(self,endpoint_url):
    sparql_query = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX owl: <http://www.w3.org/2002/07/owl#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    
    SELECT ?class ?property
    WHERE {
        ?subject ?property ?object .
        ?subject a ?class .
        FILTER (
            isIRI(?class) &&
            ?class != owl:Ontology &&
            ?class != owl:Class &&
            ?class != owl:NamedIndividual &&
            !isBlank(?property) && 
            isIRI(?property) && 
            ?property != rdf:type &&
            ?property != rdfs:label
            )
    }
    LIMIT 10
    """
    
    try:
        # Define the HTTP headers
        headers = {
            "Accept": "application/sparql-results+json"  # Specify the format of the response
        }
        
        # Send the SPARQL query to the endpoint
        response = requests.post(endpoint_url, data={"query": sparql_query}, headers=headers)

        # Check if the request was successful (status code 200)
        if response.status_code == 200:
            # Parse the JSON response
            json_response = response.json()
            # Process the results
            for binding in json_response["results"]["bindings"]:
                class_uri = binding["class"]["value"]
                property_uri = binding["property"]["value"]
                
                print(f"Class: {class_uri}, Property: {property_uri}")
                
                # Check if class_uri and property_uri are not empty
                if not class_uri or not property_uri:
                    continue
                
                # Check if class_uri already exists in cp_index
                if class_uri not in self.cp_index:
                    self.cp_index[class_uri] = {}
                
                # Check if property_uri already exists for class_uri
                if property_uri not in self.cp_index[class_uri]:
                    self.cp_index[class_uri][property_uri] = []
                
                # Append filename to the list of filenames for property_uri
                if endpoint_url not in self.cp_index[class_uri][property_uri]:
                  self.cp_index[class_uri][property_uri].append(endpoint_url)
        else:
            print("Error:", response.status_code, response.text)
            
        # # Query the SPARQL endpoint with the SPARQL query
        # self.graph.parse(self.endpoint_url, format='xml')

        # # Execute the SPARQL query and process the results
        # concept_property_pair_results = self.graph.query(sparql_query)
        # self.build_concept2property_invindex(concept_property_pair_results, self.endpoint_url)
    except Exception as e:
        print("Error:", e)
        return None
    
    return self.cp_index
  
  def get_sparql_result(self, sparql_query):
    return self.graph.query(sparql_query)
  
  def extract_concepts_property_pair(self, concept_property_pair_results):
    for row in concept_property_pair_results:
      class_uri = row["class"]
      property_uri = row["property"]
      print(f"Class: {class_uri}, Property: {property_uri}")
      
  def build_concept2property_invindex(self, concept_property_pair_results, filename):
    for cp_pair in concept_property_pair_results:
        class_uri = cp_pair["class"]
        property_uri = cp_pair["property"]
        
        # Check if class_uri and property_uri are not empty
        if not class_uri or not property_uri:
            continue
        
        # Check if class_uri already exists in cp_index
        if class_uri not in self.cp_index:
            self.cp_index[class_uri] = {}
        
        # Check if property_uri already exists for class_uri
        if property_uri not in self.cp_index[class_uri]:
            self.cp_index[class_uri][property_uri] = []
        
        # Append filename to the list of filenames for property_uri
        if filename not in self.cp_index[class_uri][property_uri]:
          self.cp_index[class_uri][property_uri].append(filename)
              
  def save_concept2property_invindex(self, file_path):
    with open(file_path, 'w') as f:
        json.dump(self.cp_index, f, indent=4)
        
# usage
if __name__ == "__main__":
  # Define the Blazegraph base URL
  blazegraph_base_url = "http://localhost:8080/blazegraph"

  # Define the namespace
  namespaces = ["namespace_1","namespace_2"]
  
  kgs = BuildKGIndex()
  
  for i in range(len(namespaces)):
    # Construct the SPARQL endpoint URL for the specified namespace
    endpoint_url = f"{blazegraph_base_url}/namespace/{namespaces[i]}/sparql"
    print(f"Start processing endpoint: {endpoint_url}")
    kgs.start_processing(endpoint_url)
  
  kgs.save_concept2property_invindex('C:/Users/printer_admin/Downloads/KGs/cp_ep_invindex.json')
  print("Completed.")
