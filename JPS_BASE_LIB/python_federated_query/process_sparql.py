from rdflib import Graph, Namespace
from rdflib.plugins.stores.sparqlstore import SPARQLStore
import os
import json
import requests
from tqdm import tqdm
from SPARQLWrapper import SPARQLWrapper, JSON, CSV

class ProcessSparql:
  
  def __init__(self):
    self.triplecount=0
        
  def run_sparql_json(self,base_url, namespace, sparql_query):
    # Construct the full URL to the Blazegraph namespace
    full_url = f"{base_url}/namespace/{namespace}/sparql"
    
    # Initialize the SPARQLWrapper with the full URL
    sparql = SPARQLWrapper(full_url)
    
    # Set the query and return format
    sparql.setQuery(sparql_query)
    sparql.setReturnFormat(JSON)
    
    try:
        # Execute the query and return the results
        results = sparql.query().convert()
        return results
    except Exception as e:
        print(f"An error occurred: {e}")
        return None
  
  def run_sparql_csv(self,base_url, namespace, sparql_query):
    # Construct the full URL to the Blazegraph namespace
    full_url = f"{base_url}/namespace/{namespace}/sparql"
    
    # Initialize the SPARQLWrapper
    # with the full URL
    sparql = SPARQLWrapper(full_url)
    
    # Set the query and return format
    sparql.setQuery(sparql_query)
    sparql.setReturnFormat(JSON)
    
    try:
        # Execute the query and return the results
        results = sparql.query().convert()
        return results
    except Exception as e:
        print(f"An error occurred: {e}")
        return None

        
# usage
if __name__ == "__main__":
  # Define the Blazegraph server's base url
  base_url = "http://localhost:8080/blazegraph"
  namespace = "namespace_all"
  # sparql_query = """
  #   PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
  #   PREFIX owl: <http://www.w3.org/2002/07/owl#>
  #   PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    
  #   SELECT ?class ?property
  #   WHERE {
  #       ?subject ?property ?object .
  #       ?subject a ?class .
  #       FILTER (
  #           isIRI(?class) &&
  #           ?class != owl:Ontology &&
  #           ?class != owl:Class &&
  #           ?class != owl:NamedIndividual &&
  #           !isBlank(?property) && 
  #           isIRI(?property) && 
  #           ?property != rdf:type &&
  #           ?property != rdfs:label
  #           )
  #   }
  #   """
  sparql_query="""
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
    
  proc = ProcessSparql()
  results=proc.run_sparql_json(base_url, namespace, sparql_query)
  
  triples=[]
  if results:
    progress_bar = tqdm(total=len(results["results"]["bindings"]), desc="Processing triples", unit="triple")
    # Process the results
    for result in results["results"]["bindings"]:
      atuple=[]
      for var in result:
        atuple.append(result[var]['value'])
        
      # Update progress bar for each triple processed
      progress_bar.update(1)
      
      if atuple not in triples:
        triples.append(atuple)
      
    progress_bar.close()
    
  print(len(triples))
  for i in range(len(triples)):
    atuple=triples[i]
    for j in range(len(atuple)):
      print(f"{atuple[j]}")
