import json
import os

from KGToolbox.Tools.GeneralTools import split_iri
from Marie.Util.location import DICTIONARY_DIR
from KGToolbox.Tools.IntegratedTrainingFileCreator import IntegratedTrainingFileCreator

ontology = "ontoagent"
sub_ontology = "pceagent"

my_file_creator = IntegratedTrainingFileCreator(sparql_namespace="copy_ontospecies_pubchem", ontology="ontoagent",
                                                sub_ontology="pceagent")

GET_ALL_SPECIES_AND_LABELS = """
prefix skos: <http://www.w3.org/2004/02/skos/core#>
prefix os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
prefix species: <http://www.theworldavatar.com/kg/ontospecies/>
SELECT DISTINCT ?species ?label ?other_label ?formula ?smile
WHERE {
  	?species rdf:type <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#Species> .
  	?species  rdfs:label ?label  ;
                                                                                              
    OPTIONAL {                                                                                         
      ?species   skos:altLabel ?other_label  .
      ?species   os:hasMolecularFormula ?formula_node .
      ?species   os:hasSMILES ?smiles_node .
      ?formula_node os:value ?formula .
      ?smiles_node os:value ?smile .

    }
                                                                          
}  
"""
name_list = []
name_dict = {}
bindings = my_file_creator.query_blazegraph(GET_ALL_SPECIES_AND_LABELS)
label_heads = bindings["head"]["vars"]
label_heads.remove("species")

binding_list = bindings["results"]["bindings"]

counter = 0
for binding in binding_list:
    counter += 1
    print(f"{counter} out of {len(binding_list)}")
    # species = split_iri(binding["species"]["value"])
    species = binding["species"]["value"]
    labels = []
    # for binding in bindings["results"]["bindings"]:
    for head in label_heads:
        if head in binding:
            label = binding[head]["value"].strip().upper()
            labels.append(label)
    labels = list(set(labels))

    name_list += labels
    for label in labels:
        if label not in name_dict:
            name_dict[label] = species

name_list = list(set(name_list))
with open(os.path.join(DICTIONARY_DIR, ontology, sub_ontology, "name_dict.json"), "w") as f:
    f.write(json.dumps(name_dict))
    f.close()

with open(os.path.join(DICTIONARY_DIR, ontology, sub_ontology, "name_list.json"), "w") as f:
    f.write(json.dumps(name_list))
    f.close()
