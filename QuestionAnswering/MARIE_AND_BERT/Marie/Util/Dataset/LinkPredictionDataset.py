import os
import pickle
import random

import pandas as pd
import torch
from torch.utils.data import DataLoader

from Marie.Util.NHopExtractor import HopExtractor
from Marie.Util.location import DATA_DIR


class LinkPredictionDataset(torch.utils.data.Dataset):

    def __init__(self, df, dataset_path=None, dataset_name=None, neg_rate=20, mode="train"):
        self.df = df
        if dataset_path is None:
            e2i_path = open(os.path.join(DATA_DIR, f'entity2idx.pkl'), 'rb')
            r2i_path = open(os.path.join(DATA_DIR, f'relation2idx.pkl'), 'rb')
        else:
            e2i_path = open(os.path.join(DATA_DIR, f'{dataset_path}/entity2idx.pkl'), 'rb')
            r2i_path = open(os.path.join(DATA_DIR, f'{dataset_path}/relation2idx.pkl'), 'rb')

        self.entity2idx = pickle.load(e2i_path)
        self.relation2idx = pickle.load(r2i_path)
        self.ent_num = len(self.entity2idx.keys())
        self.rel_num = len(self.relation2idx.keys())

        self.all_t_idx_list = range(0, self.ent_num)

        self.extractor = HopExtractor(dataset_dir=dataset_path, dataset_name=dataset_name)
        self.neg_rate = neg_rate
        self.mode = mode
        if self.mode == "train":
            self.all_triples = self.create_train_triples()
            print(f"total number of triples: {len(self.all_triples)}")
        else:
            self.h_r_t = pickle.load(open(os.path.join(dataset_path, "h_r_t.pkl"), "rb"))
            # self.df = self.df.sample(n=1000)

    def create_fake_triples(self, h_idx, r_idx, t_idx):
        neg_triples_hr_t = []
        neg_triples_h_rt = []
        neg_triples_ht_r = []
        neg_triples_h_t_r = []
        counter = 0

        while len(neg_triples_h_t_r) <= self.neg_rate:
            counter += 1
            # print(f"neg sample number {counter}")
            random_head_idx = random.randrange(0, self.ent_num)
            random_tail_idx = random.randrange(0, self.ent_num)
            triple_str = f"{random_head_idx}_{r_idx}_{random_tail_idx}"
            if not self.extractor.check_triple_existence(triple_str=triple_str):
                neg_triples_h_t_r.append((random_head_idx, r_idx, random_tail_idx))

        while len(neg_triples_hr_t) <= self.neg_rate:
            counter += 1
            # print(f"neg sample number {counter}")
            random_tail_idx = random.randrange(0, self.ent_num)
            triple_str = f"{h_idx}_{r_idx}_{random_tail_idx}"
            if not self.extractor.check_triple_existence(triple_str=triple_str):
                neg_triples_hr_t.append((h_idx, r_idx, random_tail_idx))

        while len(neg_triples_h_rt) <= self.neg_rate:
            counter += 1
            # print(f"neg sample number {counter}")
            random_head_idx = random.randrange(0, self.ent_num)
            triple_str = f"{random_head_idx}_{r_idx}_{t_idx}"
            if not self.extractor.check_triple_existence(triple_str=triple_str):
                neg_triples_h_rt.append((random_head_idx, r_idx, t_idx))

        while len(neg_triples_ht_r) <= self.neg_rate:
            counter += 1
            # print(f"neg sample number {counter}")
            random_rel_idx = random.randrange(0, self.rel_num)
            triple_str = f"{h_idx}_{random_rel_idx}_{t_idx}"
            if not self.extractor.check_triple_existence(triple_str=triple_str):
                neg_triples_ht_r.append((h_idx, random_rel_idx, t_idx))

        neg_triples = neg_triples_hr_t + neg_triples_h_rt + neg_triples_ht_r + neg_triples_h_t_r
        return neg_triples

    # def create_test_triples(self, idx, row):
    # triples = []
    # for idx, row in self.df.iterrows():
    # print(f"{idx} out out {len(self.df)}")
    # h_idx, r_idx, true_t_idx = self.entity2idx[row[0]], self.relation2idx[row[1]], self.entity2idx[row[2]]
    # for tail in self.all_t_idx_list:
    # triples.append((h_idx, r_idx, tail, true_t_idx))
    # triples.append((h_idx, r_idx, true_t_idx, true_t_idx))
    # return [h_idx, r_idx, true_t_idx]

    def create_train_triples(self):
        print("Creating all triples for training")
        all_triples = []
        for idx, row in self.df.iterrows():
            print(f"{idx} out of {len(self.df)}")
            h_idx, r_idx, t_idx = self.entity2idx[row[0]], self.relation2idx[row[1]], self.entity2idx[row[2]]
            true_triple = (h_idx, r_idx, t_idx)
            fake_triples = self.create_fake_triples(h_idx=h_idx, r_idx=r_idx, t_idx=t_idx)
            for fake_triple in fake_triples:
                all_triples.append((true_triple, fake_triple))
        return all_triples

    def __len__(self):
        if self.mode == "test":
            return len(self.df)
            # return 1000
        else:
            return len(self.all_triples)

    def __getitem__(self, item):
        if self.mode == "test":
            return self.h_r_t[item]
            # return self.create_test_triples(idx=item, row=self.df.iloc[item])
        else:
            return self.all_triples[item]


if __name__ == "__main__":
    dataset_dir = os.path.join(DATA_DIR, "CrossGraph", "fb15k")
    df_train = pd.read_csv(os.path.join(dataset_dir, "fb15k-train.txt"), sep="\t", header=None)
    df_train = df_train.sample(n=5)
    df_valid = pd.read_csv(os.path.join(dataset_dir, "fb15k-valid.txt"), sep="\t", header=None)
    df_test = pd.read_csv(os.path.join(dataset_dir, "fb15k-test.txt"), sep="\t", header=None)
    # relations = list(df_train.loc[:, 1].values)
    my_dataset = LinkPredictionDataset(df_train, dataset_path=dataset_dir, dataset_name="fb15k", neg_rate=20)
    my_dataloader = DataLoader(my_dataset, shuffle=False, batch_size=20)
    rel_num = my_dataset.rel_num

    test_dataset = LinkPredictionDataset(df_test, dataset_path=dataset_dir, dataset_name="fb15k", neg_rate=rel_num,
                                         mode="test")
    test_dataloader = DataLoader(test_dataset, shuffle=False, batch_size=1)

    for row in test_dataloader:
        head, rel, true_tail = row
        head = head.repeat(10)
        rel = rel.repeat(10)
        tail = torch.range(0, 10).long()
        print(head, rel, tail)
