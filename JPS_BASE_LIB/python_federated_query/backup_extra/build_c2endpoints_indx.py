from rdflib import Graph, Namespace, URIRef
from rdflib.plugins.stores.sparqlstore import SPARQLStore
import os
import json
import requests
from tqdm import tqdm

class BuildKGIndex:
  
  def __init__(self):
    self.triplecount=0
    self.class_index = {}
    self.graph = Graph()
    
        
  def start_processing(self,endpoint_url):
    sparql_query = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX owl: <http://www.w3.org/2002/07/owl#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    
    SELECT ?class
    WHERE {
        ?subject a ?class .
        FILTER (
            isIRI(?class) &&
            ?class != owl:Ontology &&
            ?class != owl:Class &&
            ?class != owl:NamedIndividual
            )
    }
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
            
            progress_bar = tqdm(total=len(json_response["results"]["bindings"]), desc="Processing triples", unit="triple")
            # Process the results
            for binding in json_response["results"]["bindings"]:
                class_uri = binding["class"]["value"]
                
                # print(f"Class: {class_uri}")
                
                # Check if class_uri is not empty
                if (not class_uri):
                    continue
                
                # Check if class_uri already exists in cp_index
                if class_uri not in self.class_index:
                    self.class_index[class_uri] = set()
                
                if endpoint_url not in self.class_index[class_uri]:
                  self.class_index[class_uri].add(endpoint_url)
                  
                # self.build_inverted_index(classes,file_path)
                # if endpoint_url not in self.class_index[class_uri]:
                #   self.class_index[class_uri].append(endpoint_url)
                  
                # Update progress bar for each triple processed
                progress_bar.update(1)
                
            progress_bar.close()
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
    
    return self.class_index
              
  # def save_concept2endpoint_invindex(self, file_path):
  #   with open(file_path, 'w') as f:
  #       json.dump(self.class_index, f, indent=4)
  def get_index(self):
    return self.class_index
       
  def add_triples_and_update_index(self, new_triples, endpoint_url):
        modified_classes = set()
        
        for s, p, o in new_triples:
            self.graph.add((s, p, o))
            
            if p == URIRef("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"):
                class_uri = str(o)
                
                if class_uri not in self.class_index:
                    self.class_index[class_uri] = set()
                
                self.class_index[class_uri].add(endpoint_url)
                modified_classes.add(class_uri)
                
        self.triplecount += len(new_triples)
        print(f"Added {len(new_triples)} triples and updated the index.")
        
        return modified_classes
    
  def append_index(self, file_path, modified_classes):
      if os.path.exists(file_path):
          with open(file_path, 'r') as file:
              current_index = json.load(file)
      else:
          current_index = {}
      
      for class_uri in modified_classes:
          if class_uri not in current_index:
              current_index[class_uri] = list(self.class_index[class_uri])
          else:
              current_index[class_uri] = list(set(current_index[class_uri]) | self.class_index[class_uri])
      
      with open(file_path, 'w') as file:
          json.dump(current_index, file)
      
      print(f"Index appended to {file_path}")
      
  def save_index(self, file_path):
    current_index = {}
    
    for class_uri in self.class_index:
        if class_uri not in current_index:
            current_index[class_uri] = list(self.class_index[class_uri])
        else:
            current_index[class_uri] = list(set(current_index[class_uri]) | self.class_index[class_uri])
    
    with open(file_path, 'w') as file:
        json.dump(current_index, file)
        
  def load_index(self, file_path):
      if os.path.exists(file_path):
          with open(file_path, 'r') as file:
              self.class_index = {k: set(v) for k, v in json.load(file).items()}
          print(f"Index loaded from {file_path}")
      else:
          print(f"No index file found at {file_path}")
# usage
if __name__ == "__main__":
  # Define the Blazegraph base URL
  blazegraph_base_url = "http://localhost:8080/blazegraph"

  # Define the namespace
  namespaces = ["namespace_kin","namespace_compchem"]
  
  kgs = BuildKGIndex()
  
  for i in range(len(namespaces)):
    # Construct the SPARQL endpoint URL for the specified namespace
    endpoint_url = f"{blazegraph_base_url}/namespace/{namespaces[i]}/sparql"
    print(f"Start processing endpoint: {endpoint_url}")
    kgs.start_processing(endpoint_url)
    
  # current_index=kgs.get_index()
  saved_datafile='C:/Users/printer_admin/Downloads/KGs/c2ep_invindex.json'
  kgs.save_index(saved_datafile)
  print(f"Completed and saved in {saved_datafile}.")
