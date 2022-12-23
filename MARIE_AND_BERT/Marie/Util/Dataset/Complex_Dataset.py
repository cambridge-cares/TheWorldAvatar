import os
import pickle
import random
import torch
from Marie.Util.location import DATA_DIR
from Marie.Util.NHopExtractor import HopExtractor
import pandas as pd


class ComplexDataset(torch.utils.data.Dataset):

    # the dataset takes a subset of the dataset (train, val) as the input.
    # in this case, each data unit contains a head, a rel, a tail
    # the output is the

    def __init__(self, df, data_folder=None, dataset_name=None):
        # TODO: make sure the
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
        self.all_triples = self.create_all_triples()

    def __len__(self):
        return len(self.df)

    def create_fake_triple(self, s, p, o):
        all_neighbours = self.hop_extractor.extract_neighbour_from_idx(s)
        flag = True
        while flag:
            fake_o = random.choice(all_neighbours)
            fake_triple = (s, p, fake_o, 0)
            triple_str = f'{s}_{p}_{fake_o}'
            flag = self.hop_extractor.check_triple_existence(triple_str)
            if not flag:
                return fake_triple

    def create_all_triples(self):
        triples = []
        for idx, row in self.df.iterrows():
            s = self.entity2idx[row[0]]
            p = self.relation2idx[row[1]]
            o = self.entity2idx[row[2]]
            true_triple = (s, p, o, 1)
            fake_triple = self.create_fake_triple(s, p, o)
            triples.append(true_triple)
            triples.append(fake_triple)
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
