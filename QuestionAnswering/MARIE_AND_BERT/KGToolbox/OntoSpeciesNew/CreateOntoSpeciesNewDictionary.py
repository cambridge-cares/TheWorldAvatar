import json
import os

from Marie.Util.CommonTools.FileLoader import FileLoader
from KGToolbox.Tools.IntegratedTrainingFileCreator import IntegratedTrainingFileCreator
from Marie.Util.location import DATA_DIR, DICTIONARY_DIR

ontology = "ontospecies_new"
sub_ontology = "base_full_no_pref_selected_role_limited_100"
dataset_dir = os.path.join(DATA_DIR, "CrossGraph", ontology, sub_ontology)
my_file_loader = FileLoader(full_dataset_dir=dataset_dir, dataset_name=sub_ontology)
entity2idx, idx2entity, rel2idx, idx2rel = my_file_loader.load_index_files()
triples = my_file_loader.load_all_triples()
my_file_creator = IntegratedTrainingFileCreator(sparql_namespace="copy_ontospecies_pubchem", ontology=ontology,
                                                sub_ontology=sub_ontology)
all_species = []
for triple in triples:
    s, p, o = [e.strip() for e in triple.split("\t")]
    if p == "hasUse":
        all_species.append(s)

all_species = list(set(all_species))
print(len(all_species))

GET_ALL_SPECIES_LABELS = """
prefix skos: <http://www.w3.org/2004/02/skos/core#>
prefix os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
prefix species: <http://www.theworldavatar.com/kg/ontospecies/>
SELECT DISTINCT ?label ?other_label ?formula ?smile
WHERE {
  species:%s  rdfs:label ?label  ;
                                                                                              
    OPTIONAL {                                                                                         
      species:%s   skos:altLabel ?other_label  .
      species:%s   os:hasMolecularFormula ?formula_node .
      species:%s   os:hasSMILES ?smiles_node .
      ?formula_node os:value ?formula .
      ?smiles_node os:value ?smile .
    }
                                                                          
}  
"""

all_labels = []
label_iri_dict = {"class": {"SPECIES": "species"} , "species": {}}
counter = 0
for species in all_species:
    counter += 1
    print(f"{counter} out of {len(all_species)}")
    QUERY = GET_ALL_SPECIES_LABELS % (species, species,species,species)
    bindings = my_file_creator.query_blazegraph(QUERY)
    heads = bindings["head"]["vars"]

    labels = []
    for binding in bindings["results"]["bindings"]:
        for head in heads:
            if head in binding:
                label = binding[head]["value"].strip().upper()
                labels.append(label)
    labels = list(set(labels))

    all_labels += labels
    for label in labels:
        if label not in label_iri_dict['species']:
            label_iri_dict['species'][label] = [species]

all_labels = list(set(all_labels + ["SPECIES"]))
type_dict = {"class": ["SPECIES"], "species": all_labels}


with open(os.path.join(DICTIONARY_DIR, ontology, "name_dict.json"), "w") as f:
    f.write(json.dumps(label_iri_dict))
    f.close()

with open(os.path.join(DICTIONARY_DIR, ontology, "name_list.json"), "w") as f:
    f.write(json.dumps(all_labels))
    f.close()

with open(os.path.join(DICTIONARY_DIR, ontology, "class_label_list.json"), "w") as f:
    f.write(json.dumps(["SPECIES"]))
    f.close()

with open(os.path.join(DICTIONARY_DIR, ontology, "type_dict.json"), "w") as f:
    f.write(json.dumps(type_dict))
    f.close()






