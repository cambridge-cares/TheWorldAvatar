import os
import pickle
import time
from random import choice
import torch
from Marie.Util.location import DATA_DIR


class Dataset(torch.utils.data.Dataset):

    # the dataset takes a subset of the dataset (train, val) as the input.
    # in this case, each data unit contains a head, a rel, a tail
    # the output is the

    def __init__(self, df):
        # TODO: make sure the
        e2i_path = open(os.path.join(DATA_DIR, 'entity2idx.pkl'), 'rb')
        self.entity2idx = pickle.load(e2i_path)
        r2i_path = open(os.path.join(DATA_DIR, 'relation2idx.pkl'), 'rb')
        self.relation2idx = pickle.load(r2i_path)
        self.df = df
        self.ent_num = len(self.entity2idx.keys())
        self.rel_num = len(self.relation2idx.keys())

    def __len__(self):
        return len(self.df)

    def triplet2idx(self, triplet):
        head = self.entity2idx[triplet[0]]
        rel = self.relation2idx[triplet[1]]
        tail = self.entity2idx[triplet[2]]
        fake_entity = self.get_fake_tail(triplet[0], triplet[2])
        fake_triple = choice([(head, rel, fake_entity), (fake_entity, rel, tail)])

        # for the neg set, just replace the tail with something starting with the same prefix
        return (head, rel, tail), fake_triple

    def get_fake_tail(self, head, tail):
        # return the fake tail
        # candidates = [e for e in self.entity2idx.keys() if e.startswith(head + '_') and e != tail]
        candidates = [e for e in self.entity2idx.keys()]
        return self.entity2idx[choice(candidates)]

    def create_neg_triple(self, idx):
        pass

    def __getitem__(self, idx):
        START_TIME = time.time()
        positive_set, negative_set = self.triplet2idx(self.df[idx])
        return positive_set, negative_set
