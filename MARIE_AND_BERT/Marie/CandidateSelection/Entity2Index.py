import os
import pickle
from pprint import pprint

from Marie.CandidateSelection.location import DATA_DIR

# mapping_list = ['entity2idx', 'relation2idx']
entity2idx_map = pickle.load(open(os.path.join(DATA_DIR, 'entity2idx.pkl'), 'rb'))
relation2idx_map = pickle.load(open(os.path.join(DATA_DIR, 'relation2idx.pkl'), 'rb'))


def entity2idx(entity):
    return entity2idx_map[entity]


def relation2idx(relation):
    return relation2idx_map[relation]


relation_list = list(relation2idx_map.keys())
