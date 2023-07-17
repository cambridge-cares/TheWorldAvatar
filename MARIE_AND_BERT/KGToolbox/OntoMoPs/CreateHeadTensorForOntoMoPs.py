# TODO: return three pkl lists with tensors: all CBU, MoPs, Assembly models
import os
import pickle

import torch

from KGToolbox.Tools.IntegratedTrainingFileCreator import IntegratedTrainingFileCreator
from Marie.Util.CommonTools.FileLoader import FileLoader
from Marie.Util.location import DATA_DIR

endpoint_url = "http://kg.cmclinnovations.com:81/blazegraph_geo"
ontology = "OntoMoPs"
sub_ontology = "numerical_with_implicit"
sparql_namespace = "ontomops"

full_dataset_dir = os.path.join(DATA_DIR, "CrossGraph", ontology, sub_ontology)
file_loader = FileLoader(full_dataset_dir=full_dataset_dir)
entity2idx, idx2entity, rel2idx, idx2rel = file_loader.load_index_files()
file_creator = IntegratedTrainingFileCreator(sparql_namespace=sparql_namespace,
                                             ontology=ontology, endpoint_url=endpoint_url,
                                             sub_ontology=sub_ontology, same_frac=1, other_frac=0)

GET_ALL_MOPS = """
SELECT ?head
WHERE {
    ?head rdf:type <http://www.theworldavatar.com/ontology/ontomops/OntoMOPs.owl#MetalOrganicPolyhedra> .  
}  
"""

GET_ALL_CBUS = """
SELECT ?head
WHERE {
    ?head rdf:type <http://www.theworldavatar.com/ontology/ontomops/OntoMOPs.owl#ChemicalBuildingUnit> .  
}  
"""

GET_ALL_AMS = """
SELECT ?head
WHERE {
    ?head rdf:type <http://www.theworldavatar.com/ontology/ontomops/OntoMOPs.owl#AssemblyModel> .  
}  
"""


def get_head_tensors(query):
    all_heads = []
    rst = file_creator.query_blazegraph(query=query)["results"]["bindings"]
    for binding in rst:
        iri = binding['head']['value']
        iri = iri.split('/')[-1]
        all_heads.append(entity2idx[iri])
    return torch.LongTensor(all_heads)


def write_pkl_files(data, file_name):
    file = open(os.path.join(full_dataset_dir, file_name), 'wb')
    pickle.dump(data, file)


all_mops_tensor = get_head_tensors(GET_ALL_MOPS)
all_cbus_tensor = get_head_tensors(GET_ALL_CBUS)
all_ams_tensor = get_head_tensors(GET_ALL_AMS)

head_dict = {"mops": all_mops_tensor, "cbus": all_cbus_tensor, "ams": all_ams_tensor}
write_pkl_files(head_dict, "all_heads.pkl")




