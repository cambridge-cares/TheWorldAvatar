import sys

sys.path.append("")
from KGToolbox.Utils import query_blazegraph  
from Marie.Util.location import ROOT_DIR  
import json 

def create_dictionary():
    query = """
    PREFIX OntoSpecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>    
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>    
    SELECT ?species ?label    
    WHERE {    
        ?species rdf:type OntoSpecies:Species .                
        ?species rdfs:label ?label .            
    }
    """

    results = query_blazegraph(query=query, namespace="copy_ontospecies_pubchem")
   
    # Process results
    species_map = {}
    for result in results["results"]["bindings"]:
        species_iri = result["species"]["value"]
        species_id = species_iri.split("/")[-1]
        label = result["label"]["value"]
        species_map[species_id] = label

    # print(species_map)
    with open(ROOT_DIR+"/KGToolbox/OntoSpeciesNew/species_map_v2.json", "w") as outfile:
        json.dump(species_map, outfile)
if __name__=='__main__':
    create_dictionary()

    