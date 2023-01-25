import os, json
import pickle
import random
import time
from random import choice
import torch
from tqdm import tqdm

from Marie.Util.location import DATA_DIR
from Marie.Util.NHopExtractor import HopExtractor
import pandas as pd


class ComplexDataset(torch.utils.data.Dataset):

    # the dataset takes a subset of the dataset (train, val) as the input.
    # in this case, each data unit contains a head, a rel, a tail
    # the output is the

    def __init__(self, df, data_folder=None, dataset_name=None, mode="train", neg_rate=50):
        # TODO: make sure the
        if mode == "train":
            self.neg_rate = neg_rate
        else:
            self.neg_rate = 1

        if data_folder is None:
            e2i_path = open(os.path.join(DATA_DIR, f'entity2idx.pkl'), 'rb')
            r2i_path = open(os.path.join(DATA_DIR, f'relation2idx.pkl'), 'rb')
        else:
            e2i_path = open(os.path.join(DATA_DIR, f'{data_folder}/entity2idx.pkl'), 'rb')
            r2i_path = open(os.path.join(DATA_DIR, f'{data_folder}/relation2idx.pkl'), 'rb')
        _full_dir = os.path.join(DATA_DIR, f'{data_folder}')
        self.hop_extractor = HopExtractor(dataset_dir=_full_dir, dataset_name=dataset_name)
        self.entity2idx = pickle.load(e2i_path)
        self.relation2idx = pickle.load(r2i_path)
        self.df = df
        self.ent_num = len(self.entity2idx.keys())
        self.rel_num = len(self.relation2idx.keys())
        self.candidates = [e for e in self.entity2idx.keys()]
        self.triples_path = os.path.join(DATA_DIR, f'{data_folder}/triples-{mode}.json')
        if os.path.exists(self.triples_path):
            # self.all_triples = json.loads(open(self.triples_path).read())
            pass
        else:
            print("creating the triples")
            self.all_triples = self.create_all_triples()
            # with open(self.triples_path, 'w') as f:
            # f.write(json.dumps(self.all_triples))
            # f.close()
            print("done creating the triple")

    def __len__(self):
        return len(self.all_triples)

    def create_fake_triple(self, s, p, o, mode="head"):
        all_neighbours = self.hop_extractor.extract_neighbour_from_idx(s)
        # all_neighbours = random.randint(0, self.ent_num - 1)
        if all_neighbours is None:
            return None
        flag = True
        while flag:
            fake_idx = random.sample(all_neighbours, 1)
            if mode == "head":

                fake_triple = (fake_idx, p, o, -1)
                triple_str = f'{fake_idx}_{p}_{o}'
                flag = self.hop_extractor.check_triple_existence(triple_str)
                if not flag:
                    return fake_triple

    def create_all_triples(self):
        triples = []
        for idx, row in tqdm(self.df.iterrows()):
            s = self.entity2idx[row[0]]
            p = self.relation2idx[row[1]]
            o = self.entity2idx[row[2]]
            true_triple = (s, p, o, 1)
            for i in range(self.neg_rate):
                fake_triple = self.create_fake_triple(s, p, o, mode="tail")
                if fake_triple is not None:
                    triples.append(fake_triple)
                    triples.append(true_triple)

            for i in range(self.neg_rate):
                fake_triple = self.create_fake_triple(s, p, o, mode="head")
                if fake_triple is not None:
                    triples.append(fake_triple)
                    triples.append(true_triple)

        return triples

    def __getitem__(self, idx):
        return self.all_triples[idx], self.all_triples[idx]


if __name__ == '__main__':
    full_dir = os.path.join(DATA_DIR, 'ontocompchem_calculation')
    df_train = pd.read_csv(os.path.join(full_dir, "ontocompchem_calculation-train.txt"), sep="\t", header=None)
    df_test = pd.read_csv(os.path.join(full_dir, "ontocompchem_calculation-test.txt"), sep="\t", header=None)

    # train_set = Dataset(df_train, data_folder=full_dir)
    test_set = ComplexDataset(df_test, data_folder=full_dir)
    test_dataloader = torch.utils.data.DataLoader(test_set, batch_size=32, shuffle=True)
    for x in test_dataloader:
        print(x)
