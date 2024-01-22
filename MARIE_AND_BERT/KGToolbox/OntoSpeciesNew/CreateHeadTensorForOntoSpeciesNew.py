import os
import pickle

import torch

from KGToolbox.Tools.IntegratedTrainingFileCreator import IntegratedTrainingFileCreator
from Marie.Util.CommonTools.FileLoader import FileLoader
from Marie.Util.location import DATA_DIR

ontology = "ontospecies_new"
sub_ontology = "base_full_no_pref_selected_role_limited_100"
sparql_namespace = "copy_ontospecies_pubchem"

full_dataset_dir = os.path.join(DATA_DIR, "CrossGraph", ontology, sub_ontology)
file_loader = FileLoader(full_dataset_dir=full_dataset_dir)
entity2idx, idx2entity, rel2idx, idx2rel = file_loader.load_index_files()
file_creator = IntegratedTrainingFileCreator(sparql_namespace=sparql_namespace,
                                             ontology=ontology,
                                             sub_ontology=sub_ontology, same_frac=1, other_frac=0)


def get_head_tensors(query):
    all_heads = []
    rst = file_creator.query_blazegraph(query=query)["results"]["bindings"]
    for binding in rst:
        iri = binding['head']['value']
        iri = iri.split('/')[-1]
        if iri in entity2idx:
            all_heads.append(entity2idx[iri])
    return torch.LongTensor(all_heads)


def write_pkl_files(data, file_name):
    file = open(os.path.join(full_dataset_dir, file_name), 'wb')
    pickle.dump(data, file)


GET_ALL_SPECIES = """
SELECT  DISTINCT ?head
WHERE {
    ?head rdf:type <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#Species> .  
}   
"""

all_species_heads = {"species": get_head_tensors(GET_ALL_SPECIES)}
print(len(all_species_heads))
write_pkl_files(all_species_heads, "all_heads.pkl")
