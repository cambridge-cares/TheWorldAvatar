from rdflib import Graph, Namespace, URIRef
from rdflib.plugins.stores.sparqlstore import SPARQLStore
import os
import json
import requests
from tqdm import tqdm

class BuildKGIndex:
  
  def __init__(self):
      self.class_index_file_path=""
      self.property_index_file_path=""
      self.cp_index_file_path=""
      
      self.triplecount=0
      self.class_index = {}
      self.property_index = {}
      self.cp_index = {}
      self.graph = Graph()
   
  def set_index_location(self,index_dir):
      if(index_dir.strip()[-1]=="/"):
          index_dir=index_dir.strip()
      else:
          index_dir=index_dir.strip()+"/"
          
      self.class_index_file_path= index_dir+"cinv.indx"
      self.property_index_file_path=index_dir+"pinv.indx"
      self.cp_index_file_path=index_dir+"cpinv.indx"    
        
  def start_processing(self,endpoint_url):
    sparql_query = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX owl: <http://www.w3.org/2002/07/owl#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

    SELECT ?class ?property
    WHERE {
        {
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
                ?property != rdfs:label && 
                ?property != rdfs:comment
            )
        }
        UNION
        {
            ?subject ?property ?object .
            ?object a ?class .
            FILTER (
                isIRI(?class) &&
                ?class != owl:Ontology &&
                ?class != owl:Class &&
                ?class != owl:NamedIndividual &&
                !isBlank(?property) &&
                isIRI(?property) &&
                ?property != rdf:type &&
                ?property != rdfs:label &&
                ?property != rdfs:comment
            )
        }
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
                property_uri = binding["property"]["value"]
                
                # Creating Class-to-endpoint inverted index
                if (class_uri):    
                  if class_uri not in self.class_index:
                      self.class_index[class_uri] = []
                  
                  if endpoint_url not in self.class_index[class_uri]:
                    self.class_index[class_uri].append(endpoint_url)
                  
                # Creating Property-to-endpoint inverted index
                if (property_uri):
                  if property_uri not in self.property_index:
                      self.property_index[property_uri] = []
                  
                  if endpoint_url not in self.property_index[property_uri]:
                    self.property_index[property_uri].append(endpoint_url)
                  
                # Creating Class-Property-to-endpoint inverted index
                if ((not class_uri) or (not property_uri)):
                    continue
                  
                if class_uri not in self.cp_index:
                    self.cp_index[class_uri] = {}
                
                # Check if property_uri already exists for class_uri
                if property_uri not in self.cp_index[class_uri]:
                    self.cp_index[class_uri][property_uri] = []
                
                # Append filename to the list of filenames for property_uri
                if endpoint_url not in self.cp_index[class_uri][property_uri]:
                  self.cp_index[class_uri][property_uri].append(endpoint_url)
                  
                # Update progress bar for each triple processed
                progress_bar.update(1)
                
            progress_bar.close()
        else:
            print("Error:", response.status_code, response.text)
            
        # # Query the SPARQL endpoint with the SPARQL query
        # self.graph.parse(self.endpoint_url, format='xml')
    except Exception as e:
        print("Error:", e)
        return None
    
    return self.class_index, self.property_index, self.cp_index
  
  #get indices
  def get_c2e_index(self):
    return self.class_index
  
  def get_p2e_index(self):
    return self.property_index
  
  def get_cp2e_index(self):
    return self.cp_index
  
  #update index on new triples insertion
  def add_triples_and_update_index(self, new_triples, endpoint_url):
      #modified_classes = []
      #modified_properties = []
      #modified_cp = set()
      
      for s, p, o in new_triples:
          self.graph.add((s, p, o))
          
          if p == URIRef("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"):
              class_uri = str(o)
              last_token=self.get_substring_from_last(class_uri)
              if(last_token=="Ontology" or last_token=="Class" or last_token=="NamedIndividual"):
                  continue
              
              if class_uri not in self.class_index:
                  self.class_index[class_uri] = []
              
              self.class_index[class_uri].append(endpoint_url)
              #if class_uri not in modified_classes:
              #    modified_classes.append(class_uri)
          else:
              property_uri = str(p)
              last_token=self.get_substring_from_last(property_uri)
              if(last_token=="label"):
                  continue
                  
              if property_uri not in self.property_index:
                  self.property_index[property_uri] = []
              
              self.property_index[property_uri].append(endpoint_url)
              #if property_uri not in modified_properties:
              #    modified_properties.append(property_uri)
      
      for s, p, o in new_triples:
          if(str(p) in self.property_index):
              property_uri=str(p)
              if (str(s) in self.class_index):
                  class_uri=str(s)
                  
                  if class_uri not in self.cp_index:
                      self.cp_index[class_uri] = {}
                  if property_uri not in self.cp_index[class_uri]:
                      self.cp_index[class_uri][property_uri] = []
                  if endpoint_url not in self.cp_index[class_uri][property_uri]:
                      self.cp_index[class_uri][property_uri].append(endpoint_url)
            
              if(str(o) in self.class_index):
                  class_uri=str(o)
                  
                  if class_uri not in self.cp_index:
                      self.cp_index[class_uri] = {}
                  if property_uri not in self.cp_index[class_uri]:
                      self.cp_index[class_uri][property_uri] = []
                  if endpoint_url not in self.cp_index[class_uri][property_uri]:
                      self.cp_index[class_uri][property_uri].append(endpoint_url)
              
      self.triplecount += len(new_triples)
      print(f"Added {len(new_triples)} triples and updated the index.")
      
      #return modified_classes
  
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
  
  #Save index file
  def save_indices(self):
    self.save_class_index()
    self.save_property_index()
    self.save_cp_index()
    
  def save_class_index(self):
    with open(self.class_index_file_path, 'w') as file:
        json.dump(self.class_index, file, indent=4)
        
  def save_property_index(self):
    with open(self.property_index_file_path, 'w') as file:
        json.dump(self.property_index, file, indent=4)
  
  def save_cp_index(self):
    with open(self.cp_index_file_path, 'w') as file:
        json.dump(self.cp_index, file, indent=4)
            
  def get_substring_from_last(self,string):
        return string.rsplit('/', 1)[-1].rsplit('#', 1)[-1]
        
# usage
if __name__ == "__main__":
  # Define the Blazegraph base URL
  blazegraph_base_url = "http://localhost:8080/blazegraph"

  # Define the namespace
  #namespaces = ["namespace_kin","namespace_compchem","namespace_uken"]
  namespaces = ["namespace_all"]
  index_location='C:/Users/printer_admin/Downloads/KGs/tests'
  
  kgs = BuildKGIndex()
  kgs.set_index_location(index_location)
  
  for i in range(len(namespaces)):
    # Construct the SPARQL endpoint URL for the specified namespace
    endpoint_url = f"{blazegraph_base_url}/namespace/{namespaces[i]}/sparql"
    print(f"Start processing endpoint: {endpoint_url}")
    kgs.start_processing(endpoint_url)
  
  kgs.save_indices()
  print(f"Completed and index-files are saved in {index_location}.")
