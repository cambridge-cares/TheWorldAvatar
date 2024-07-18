# go to wikidata_numerical to find the indices of all species ..
import os
import pickle

import torch

from Marie.Util.NHopExtractor import HopExtractor
from Marie.Util.location import DATA_DIR

dataset_dir = "CrossGraph/wikidata_numerical"
triple_path = os.path.join(DATA_DIR, dataset_dir, "wikidata_numerical-train.txt")
e2i_file = open(os.path.join(DATA_DIR, dataset_dir, "entity2idx.pkl"), 'rb')
entity2idx = pickle.load(e2i_file)

subgraph_extractor = HopExtractor(dataset_dir=dataset_dir, dataset_name="wikidata_numerical")

triples = open(triple_path).readlines()
unique_species = []
all_neighbours = []
for triple in triples:
    s, p, o = [e.strip() for e in triple.split("\t")]
    if s not in unique_species:
        unique_species.append(s)
        s_idx = entity2idx[s]
        s_neighbours = subgraph_extractor.extract_neighbour_from_idx(s_idx)
        all_neighbours += s_neighbours

unique_species_idx = torch.LongTensor(sorted([entity2idx[s] for s in unique_species]))

head_dict = {"species": unique_species_idx}
with open(os.path.join(DATA_DIR, dataset_dir, 'all_heads.pkl'), 'wb') as handle:
    pickle.dump(unique_species_idx, handle, protocol=pickle.HIGHEST_PROTOCOL)
#
#
# print(len(all_neighbours))
