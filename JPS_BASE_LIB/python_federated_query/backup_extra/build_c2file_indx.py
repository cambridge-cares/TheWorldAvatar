from rdflib import Graph
import os
import json

class BuildKGIndex:
  
  def __init__(self, directory):
    self.filecount=0
    self.triplecount=0
    self.class_index = {}
    self.directory=directory
  
  def start_processing(self):
    for root, dirs, files in os.walk(self.directory): #walks along files and sub-folders
      for filename in files:
          if filename.endswith(".owl"):  # Assuming files are owl files
              file_path = os.path.join(root, filename)
              self.filecount+=1
              print(f"{self.filecount}. Processing: {file_path}")
              classes=self.extract_concepts_from_owl_file(file_path)
              self.build_inverted_index(classes,file_path)
    return self.class_index
  
  def extract_concepts_from_owl_file(self,owl_file):
    graph = Graph()
    graph.parse(owl_file, format="xml")  # Adjust format if needed
    rdf_type = '#type'
    triple_counter = 0
    classes = set()
    # Extract classes (concepts)
    for subject, predicate, obj in graph: #triples in graph 
        if predicate.endswith(rdf_type) and not obj.endswith(rdf_type):
            classes.add(obj)
            triple_counter+=1
    print(f"Processed {triple_counter} triples from file {owl_file}")
    self.triplecount+=triple_counter
    return classes
  
  def build_inverted_index(self,classes,filename):
    for class_name in classes:
        if class_name not in self.class_index:
            self.class_index[class_name] = []
        self.class_index[class_name].append(filename)
  
  def save_inverted_index(self, inverted_index, file_path):
    with open(file_path, 'w') as f:
        json.dump(inverted_index, f, indent=4)
        
  def get_triplecount(self):
    return self.triplecount
    
# usage
if __name__ == "__main__":
  directory = "C:/Users/printer_admin/Downloads/KGs/"
  kgs = BuildKGIndex(directory)
  kg_index=kgs.start_processing()
  kgs.save_inverted_index(kg_index, os.path.join(directory, 'inverted_index.json'))
  total_triple_processed=kgs.get_triplecount()
  print(f"Completed. Total Number of Processed Triples: {total_triple_processed}")

  # for class_name, related_files in kg_index.items():
  #   print(f"Class: {class_name}")
  #   print(f"Related Files: {related_files}")
  #   print()
    
  