from rdflib import Graph, Namespace
from rdflib.plugins.stores.sparqlstore import SPARQLStore
import os
import json
import requests
from tqdm import tqdm

class BuildP2EIndex:
  
  def __init__(self):
    self.triplecount=0
    self.property_index = {}
    self.graph = Graph()
    
        
  def start_processing(self,endpoint_url):
    sparql_query = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX owl: <http://www.w3.org/2002/07/owl#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    
    SELECT ?property
    WHERE {
        ?subject ?property ?object .
        FILTER (
            !isBlank(?property) && 
            isIRI(?property) && 
            ?property != rdf:type &&
            ?property != rdfs:label
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
                property_uri = binding["property"]["value"]
                
                # print(f"property: {property_uri}")
                
                # Check if property_uri is not empty
                if (not property_uri):
                    continue
                
                # Check if property_uri already exists in cp_index
                if property_uri not in self.property_index:
                    self.property_index[property_uri] = []
                
                if endpoint_url not in self.property_index[property_uri]:
                  self.property_index[property_uri].append(endpoint_url)
                  
                # self.build_inverted_index(propertyes,file_path)
                # if endpoint_url not in self.property_index[property_uri]:
                #   self.property_index[property_uri].append(endpoint_url)
                  
                # Update progress bar for each triple processed
                progress_bar.update(1)
                
            progress_bar.close()
        else:
            print("Error:", response.status_code, response.text)
            
        # # Query the SPARQL endpoint with the SPARQL query
        # self.graph.parse(self.endpoint_url, format='xml')

        # # Execute the SPARQL query and process the results
        # property_property_pair_results = self.graph.query(sparql_query)
        # self.build_property2property_invindex(property_property_pair_results, self.endpoint_url)
    except Exception as e:
        print("Error:", e)
        return None
    
    return self.property_index
  
  def get_sparql_result(self, sparql_query):
    return self.graph.query(sparql_query)
              
  def save_property2endpoint_invindex(self, file_path):
    with open(file_path, 'w') as f:
        json.dump(self.property_index, f, indent=4)
        
# usage
if __name__ == "__main__":
  # Define the Blazegraph base URL
  blazegraph_base_url = "http://localhost:8080/blazegraph"

  # Define the namespace
  namespaces = ["namespace_1","namespace_2"]
  
  kgs = BuildP2EIndex()
  
  for i in range(len(namespaces)):
    # Construct the SPARQL endpoint URL for the specified namespace
    endpoint_url = f"{blazegraph_base_url}/namespace/{namespaces[i]}/sparql"
    print(f"Start processing endpoint: {endpoint_url}")
    kgs.start_processing(endpoint_url)
  
  saved_datafile='C:/Users/printer_admin/Downloads/KGs/p2e_invindex.json'
  kgs.save_property2endpoint_invindex(saved_datafile)
  print(f"Completed and saved in {saved_datafile}.")
