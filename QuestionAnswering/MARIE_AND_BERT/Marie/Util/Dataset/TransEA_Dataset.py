import json
import os
import pickle
import random
import time
from random import choice

import numpy as np
import torch
from Marie.Util.location import DATA_DIR
from Marie.Util.NHopExtractor import HopExtractor


class Dataset(torch.utils.data.Dataset):

    # the dataset takes a subset of the dataset (train, val) as the input.
    # in this case, each data unit contains a head, a rel, a tail
    # the output is the

    def __init__(self, df, neg_rate=20, dataset_path=None, is_numerical=False, dataset_name=None):
        # TODO: make sure the
        self.neg_rate = neg_rate
        if dataset_path is None:
            e2i_path = open(os.path.join(DATA_DIR, f'entity2idx.pkl'), 'rb')
            r2i_path = open(os.path.join(DATA_DIR, f'relation2idx.pkl'), 'rb')
        else:
            e2i_path = open(os.path.join(DATA_DIR, f'{dataset_path}/entity2idx.pkl'), 'rb')
            r2i_path = open(os.path.join(DATA_DIR, f'{dataset_path}/relation2idx.pkl'), 'rb')
            if is_numerical:
                hash_numerical_path = os.path.join(DATA_DIR, f'{dataset_path}/node_value_dict.json')
                self.hash_numerical_dict = json.loads(open(hash_numerical_path).read())

        self.hop_extractor = HopExtractor(dataset_dir=os.path.join(DATA_DIR, dataset_path), dataset_name=dataset_name)
        self.entity2idx = pickle.load(e2i_path)
        self.relation2idx = pickle.load(r2i_path)
        self.df = df
        self.ent_num = len(self.entity2idx.keys())
        self.rel_num = len(self.relation2idx.keys())
        self.candidates = [e for e in self.entity2idx.keys()]
        self.is_numerical = is_numerical
        self.triples = self.create_triples()

    def __len__(self):
        return len(self.triples)

    def multidimensional_shifting(self, num_samples, sample_size, elements, probabilities):
        # replicate probabilities as many times as `num_samples`
        replicated_probabilities = np.tile(probabilities, (num_samples, 1))
        # get random shifting numbers & scale them correctly
        random_shifts = np.random.random(replicated_probabilities.shape)
        random_shifts /= random_shifts.sum(axis=1)[:, np.newaxis]
        # shift by numbers & find largest (by finding the smallest of the negative)
        shifted_probabilities = random_shifts - replicated_probabilities
        return np.argpartition(shifted_probabilities, sample_size, axis=1)[:, :sample_size]

    def create_triples(self):

        all_triples = []
        counter = 0
        for triplet in self.df:
            counter += 1
            print(f"{counter} out of {len(self.df)}")
            head = self.entity2idx[triplet[0]]
            rel = self.relation2idx[triplet[1]]
            tail = self.entity2idx[triplet[2]]
            all_neighbours = self.hop_extractor.extract_neighbour_from_idx(head)
            all_neighbours.remove(tail)
            for fake_tail in all_neighbours:
                fake_triple = (head, rel, fake_tail)
                if self.is_numerical:
                    numerical_value = self.hash_numerical_dict[triplet[2]]  # / 100
                else:
                    numerical_value = []
                triple = ((head, rel, tail), fake_triple, numerical_value)
                all_triples.append(triple)

        return all_triples

    def triplet2idx(self, triplet):
        head = self.entity2idx[triplet[0]]
        rel = self.relation2idx[triplet[1]]
        tail = self.entity2idx[triplet[2]]
        fake_entity = random.randint(0, self.ent_num)
        fake_triple = (head, rel, fake_entity)
        # for the neg set, just replace the tail with something starting with the same prefix
        if self.is_numerical:
            numerical_value = self.hash_numerical_dict[triplet[2]]
        else:
            numerical_value = []
        return (head, rel, tail), fake_triple, numerical_value

    def __getitem__(self, idx):
        # START_TIME = time.time()
        # positive_set, negative_set, numerical_list = self.triplet2idx(self.df[idx])
        # print('Used time: ', time.time() - START_TIME)
        # return positive_set, negative_set, numerical_list
        return self.triples[idx]


if __name__ == '__main__':
    dataset_path = "CrossGraph/wikidata_single"
    dataset_name = "wikidata_single"

    train_triplets_numerical = [line.split('\t') for line in
                                open(os.path.join(DATA_DIR, dataset_path,
                                                  f'{dataset_name}_numerical-train.txt')).read().splitlines()]

    test_triplets_numerical = [line.split('\t') for line in
                               open(os.path.join(DATA_DIR, dataset_path,
                                                 f'{dataset_name}_numerical-test.txt')).read().splitlines()]

    train_numerical_set = Dataset(train_triplets_numerical, dataset_path=dataset_path, is_numerical=True)
    test_numerical_set = Dataset(train_triplets_numerical, dataset_path=dataset_path, is_numerical=True)
    train_numerical_dataloader = torch.utils.data.DataLoader(train_numerical_set, batch_size=32, shuffle=True)
    test_numerical_dataloader = torch.utils.data.DataLoader(test_numerical_set, batch_size=32, shuffle=True)

    train_triplets_non_numerical = [line.split('\t') for line in
                                    open(os.path.join(DATA_DIR, dataset_path,
                                                      f'{dataset_name}_non_numerical-train.txt')).read().splitlines()]

    test_triplets_non_numerical = [line.split('\t') for line in
                                   open(os.path.join(DATA_DIR, dataset_path,
                                                     f'{dataset_name}_non_numerical-test.txt')).read().splitlines()]

    train_non_numerical_set = Dataset(train_triplets_non_numerical, dataset_path=dataset_path, is_numerical=False)
    test_non_numerical_set = Dataset(train_triplets_non_numerical, dataset_path=dataset_path, is_numerical=False)
    train_non_numerical_dataloader = torch.utils.data.DataLoader(train_non_numerical_set, batch_size=32, shuffle=True)
    test_non_numerical_dataloader = torch.utils.data.DataLoader(test_non_numerical_set, batch_size=32, shuffle=True)
    for x in test_numerical_dataloader:
        print(x)
