from rdflib import Graph
import os
import json
import warnings

class BuildKGIndex:
  
  def __init__(self, directory):
    self.filecount=0
    self.triplecount=0
    self.class_index = {}
    self.cp_index = {}
    self.directory=directory
    self.graph=Graph()
    warnings.filterwarnings("ignore", message=".*does not look like a valid URI.*")
    
  def __del__(self):
    warnings.resetwarnings()
        
  def start_processing(self):
    sparql_query = """
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
    """
    
    for root, dirs, files in os.walk(self.directory): #walks along files and sub-folders
      for filename in files:
          if filename.endswith(".owl"):  # Assuming files are owl files
              file_path = os.path.join(root, filename)
              self.filecount+=1
              print(f"{self.filecount}. Processing: {file_path}")
              self.create_graph_from_owl_file(file_path)
              concept_property_pair_results=self.get_sparql_result(sparql_query)
              self.build_concept2property_invindex(concept_property_pair_results, file_path)

    return self.class_index
  
  def create_graph_from_owl_file(self,owl_file):
    self.graph = Graph()
    self.graph.parse(owl_file, format="xml")  # Adjust format if needed
  
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
  directory = "C:/Users/printer_admin/Downloads/KGs/"
  kgs = BuildKGIndex(directory)
  kg_index=kgs.start_processing()
  kgs.save_concept2property_invindex(os.pa0th.join(directory, 'cp_invindex.json'))
  print(f"Completed.")

  # for class_name, related_files in kg_index.items():
  #   print(f"Class: {class_name}")
  #   print(f"Related Files: {related_files}")
  #   print()
    
  