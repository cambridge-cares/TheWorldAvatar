import json
import os

import pandas as pd
import torch

from Marie.Util.CommonTools.FileLoader import FileLoader
from Marie.Util.location import DATA_DIR


class TransRInferenceDataset(torch.utils.data.Dataset):
    """

    """

    def __init__(self, df, full_dataset_dir, ontology, mode="train", test_reaction_triples=None):
        super(TransRInferenceDataset, self).__init__()
        self.full_dataset_dir = full_dataset_dir
        self.ontology = ontology
        self.file_loader = FileLoader(full_dataset_dir=self.full_dataset_dir, dataset_name=self.ontology)
        self.entity2idx, self.idx2entity, self.rel2idx, self.idx2rel = self.file_loader.load_index_files()
        self.neg_sample_dict_path = os.path.join(self.full_dataset_dir, "neg_sample_dict.json")
        self.neg_sample_dict = json.loads(open(self.neg_sample_dict_path).read())
        self.mode = mode
        self.df = df
        if self.mode == "train":
            self.triples = self.create_all_triples()
            print(f"Number of triples for training: {len(self.triples)}")
        else:
            self.triples = self.create_test_triples()
            print(f"Number of triples for testing: {len(self.triples)}")

        self.ent_num = len(self.entity2idx.keys())
        self.rel_num = len(self.rel2idx.keys())

    def create_fake_triple(self, s, p, o, mode="head"):
        s_p_str = f'{s}_{p}'
        fake_candidates = self.neg_sample_dict[s_p_str]
        return fake_candidates

    def create_test_triples(self):
        triples = []
        for idx, row in self.df.iterrows():
            # print(f"{idx} out of {len(self.df)}")
            s = self.entity2idx[row[0]]
            p = self.rel2idx[row[1]]
            o = self.entity2idx[row[2]]
            true_triple = (s, p, o)
            triples.append(true_triple)
        return triples

    def create_all_triples(self):
        triples = []
        for idx, row in self.df.iterrows():
            # print(f"{idx} out of {len(self.df)}")
            s = self.entity2idx[row[0]]
            p = self.rel2idx[row[1]]
            o = self.entity2idx[row[2]]
            true_triple = (s, p, o)
            fake_tails = self.create_fake_triple(s, p, o, mode="head")
            for fake_tail in fake_tails:
                fake_triple = (s, p, fake_tail)
                triples.append((true_triple, fake_triple))

        return triples

    def create_inference_test_sample(self, reaction_triples):
        """
        Given the triples for test, we try the following evaluation, use the reactants embeddings r to calculate the
        embedding of the reaction R' which is not observed in the training set.
        Then use R' to infer the products ...
        Then reverse the process to infer reactants given products ...
        :return: list of tuples (S_r1, isReactant), (S_r2, isReactant) -> (S_p1), (S_p2)
        """
        triples = []
        for reaction in reaction_triples:
            data = reaction_triples[reaction]
            r_idx_list = [self.entity2idx[r] for r in data['reactants']]
            p_idx_list = [self.entity2idx[p] for p in data['products']]
            triples.append((r_idx_list, p_idx_list))
        return triples

    def __len__(self):
        return len(self.triples)

    def __getitem__(self, item):
        return self.triples[item]


if __name__ == "__main__":
    full_dir = os.path.join(DATA_DIR, 'CrossGraph', 'ontospecies_new/role_only')
    df_train = pd.read_csv(os.path.join(full_dir, "role_only-train-2.txt"), sep="\t", header=None)
    df_test = pd.read_csv(os.path.join(full_dir, "role_only-test.txt"), sep="\t", header=None)
    train_set = TransRInferenceDataset(df_train, full_dataset_dir=full_dir, ontology="role_only", mode="train")
    test_set = TransRInferenceDataset(df_test, full_dataset_dir=full_dir, ontology="role_only", mode="test")
    test_dataloader = torch.utils.data.DataLoader(test_set, batch_size=1, shuffle=True)
    train_dataloader = torch.utils.data.DataLoader(train_set, batch_size=1, shuffle=True)

    # for row in train_dataloader:
    #     print(row)

    # for row in test_dataloader:
    #     reactants = [s.item() for s in row[0]]
    #     products = [s.item() for s in row[1]]
    #     print(reactants)
    #     print(products)
    #     print("--------")
