import os, sys
import pickle
import random
from random import choice

import numpy as np
import pandas as pd
from tqdm import tqdm

sys.path.append('../../../')
import torch
import torch.nn as nn
from Marie.Util.location import DATA_DIR


class PlattCalibration(nn.Module):

    def __init__(self):
        super(PlattCalibration, self).__init__()

    def forward(self):
        """
        platt model contains two variables

        :return: the loss is platt scaled distance of the triple ...
        """


class PlattTrainer:
    pass


class Dataset(torch.utils.data.Dataset):

    def __init__(self, df, from_scratch=False):
        # TODO: load the training set and other indexings
        self.df = df
        e2i_path = open(os.path.join(DATA_DIR, 'entity2idx.pkl'), 'rb')
        self.entity2idx = pickle.load(e2i_path)
        r2i_path = open(os.path.join(DATA_DIR, 'relation2idx.pkl'), 'rb')
        self.relation2idx = pickle.load(r2i_path)
        self.all_rel = list(set(self.relation2idx.keys()))
        self.all_rel.remove('type')
        self.all_ent = list(set(self.entity2idx.keys()))
        self.all_heads = [e for e in self.all_ent if '_' not in e]
        self.all_tails = [e for e in self.all_ent if '_' in e]
        self.ent_num = len(self.all_ent)
        self.rel_num = len(self.all_rel)

        # # TODO: insert label 1 to all the given real triples
        # self.df.insert(3, 3, 1)
        # if from_scratch:
        #     self.complete_dataset()
        #     self.df.to_csv(os.path.join(DATA_DIR, 'pubchem5000.tsv'), sep='\t')
        # # TODO: create the negative set
        # # TODO: concat the two sets

        use_cuda = torch.cuda.is_available()
        self.device = torch.device("cuda" if use_cuda else "cpu")

        self.Y = self.df.iloc[:, 3:]
        self.X = self.df.iloc[:, 0:3]

    def __len__(self):
        return len(self.df)

    def complete_dataset(self):
        """
        :param df: given one positive triplet, generate one negative triplet where the tails are nearby the real one
        :return:
        """
        all_fake_rows = []
        # e.g. CID1 charge CID1_charge
        for index, row in self.df.iterrows():
            head = row[0]
            rel = row[1]
            tail = row[2]
            # create a fake head, create a fake tail
            fake_tail = choice([head + '_' + _rel for _rel in self.all_rel if rel != _rel and 'type' not in rel])
            fake_head = choice(self.all_heads)
            fake_row = row
            if random.random() < 0.5:
                # replace head
                fake_row = fake_row.replace(head, fake_head)
            else:
                # replace tail
                fake_row = fake_row.replace(tail, fake_tail)
            fake_row = fake_row.replace(1, 0)
            all_fake_rows.append(fake_row)
        self.df = pd.concat([pd.DataFrame(all_fake_rows), self.df]).reset_index(drop=True)
        for index, row in self.df.iterrows():
            head, rel, tail = row[0], row[1], row[2]
            head_idx, rel_idx, tail_idx = self.entity2idx[head], self.relation2idx[rel], self.entity2idx[tail]
            self.df.iloc[index] = row.replace(head, head_idx).replace(rel, rel_idx).replace(tail, tail_idx)

    def __getitem__(self, idx):
        """
        Allows retrieval of training/testing item
        :param idx:
        :return: return the triplet (and fake triples) with their label, where 1 is real, and 0 is fake
        """
        data_x = self.X.iloc[[idx]].values[0]
        data_y = self.Y.iloc[[idx]].values[0]

        return torch.LongTensor(data_x), torch.FloatTensor(data_y).to(self.device)


class TransE_Proba(nn.Module):
    """
    The purpose this class is to train a TransE with the ability to return the
    probability of a triple, instead of the distance of the difference between
    inferred tail and the given tail
    """

    def __init__(self, dim, ent_num, rel_num, device, resume_training=False):
        super(TransE_Proba, self).__init__()
        self.dim = dim
        self.norm = 1
        self.ent_num = ent_num
        self.rel_num = rel_num
        self.device = device
        self.ent_embedding = self._init_ent_embedding()
        self.rel_embedding = self._init_rel_embedding()
        if resume_training:
            self.ent_embedding = self.load_ent_embedding()
            self.rel_embedding = self.load_rel_embedding()

        self.sigmoid = nn.Sigmoid()
        self.softmax = nn.Softmax()
        self.the_linear_layer = nn.Linear(50, 1)

        self.linear = nn.Linear(50, 50)

    def load_ent_embedding(self):
        tsv_file_ent = pd.read_csv(os.path.join(DATA_DIR, 'ent_embedding.tsv'), sep='\t')
        pretrained_ent_embedding = torch.FloatTensor(tsv_file_ent.values)
        self.ent_embedding = nn.Embedding.from_pretrained(pretrained_ent_embedding).requires_grad_(True)
        return self.ent_embedding

    def load_rel_embedding(self):
        tsv_file_rel = pd.read_csv(os.path.join(DATA_DIR, 'rel_embedding.tsv'), sep='\t')
        pretrained_rel_embedding = torch.FloatTensor(tsv_file_rel.values)
        self.rel_embedding = nn.Embedding.from_pretrained(pretrained_rel_embedding).requires_grad_(True)
        # self.rel_embedding.weight.data[:-1, :].div_(self.rel_embedding.weight.data[:-1, :].norm(p=1, dim=1, keepdim=True))

        return self.rel_embedding

    def _init_ent_embedding(self):
        ent_embedding = nn.Embedding(embedding_dim=self.dim, num_embeddings=self.ent_num + 1, padding_idx=self.ent_num)
        ent_embedding.weight.data.uniform_(-1, 1)
        return ent_embedding

    def _init_rel_embedding(self):
        rel_embedding = nn.Embedding(embedding_dim=self.dim, num_embeddings=self.rel_num + 1, padding_idx=self.rel_num)
        rel_embedding.weight.data.uniform_(-1, 1)
        return rel_embedding

    def forward(self, triplets):
        """ Where you train the model
        :param triplets:
        :return: still return the distance, but the loss function is BCE
        """
        distance = self.distance(triplets)
        # distance = self.the_linear_layer(distance)
        # distance = torch.squeeze(distance, 1)
        # distance = self.softmax(distance)
        distance = self.sigmoid(distance )
        return distance

    def distance(self, triplet):
        """
        Where you measure the distance between head + rel and tail
        :param triplet:
        :return: the norm of distance between the derived tail and the given tail
        """
        head = self.ent_embedding(triplet[0].to(self.device)).to(self.device)
        rel = self.rel_embedding(triplet[1].to(self.device)).to(self.device)
        tail = self.ent_embedding(triplet[2].to(self.device)).to(self.device)

        # head = self.linear(head.type(torch.FloatTensor).to(self.device)).to(self.device)
        # rel = self.linear(rel.type(torch.FloatTensor).to(self.device)).to(self.device)
        # tail = self.linear(tail.type(torch.FloatTensor).to(self.device)).to(self.device)

        return (head + rel - tail).norm(p=self.norm, dim=1).to(self.device)


class Trainer:

    def export_embeddings(self):
        ent_lines = []

        for embedding in self.model.ent_embedding.weight.data:
            e_line = '\t'.join([str(e) for e in embedding.tolist()])
            ent_lines.append(e_line)
        ent_content = '\n'.join(ent_lines)
        with open(os.path.join(DATA_DIR, 'with_score/ent_embedding.tsv'), 'w') as f:
            f.write(ent_content)
            f.close()

        rel_lines = []
        for embedding in self.model.rel_embedding.weight.data:
            r_line = '\t'.join([str(r) for r in embedding.tolist()])
            rel_lines.append(r_line)
        rel_content = '\n'.join(rel_lines)
        with open(os.path.join(DATA_DIR, 'with_score/rel_embedding.tsv'), 'w') as f:
            f.write(rel_content)
            f.close()

    def save_model(self):
        torch.save(self.model.state_dict(), '../../../Training/Embedding/playground/model')
        self.export_embeddings()
        print(f'saving the model and the embeddings')

    def __init__(self):
        """

        """
        self.batch_size = 64
        self.epochs = 100
        self.learning_rate = 0.5 # 5e-5
        self.step = 0

        self.dataset_name = 'pubchem5000'
        self.df = pd.read_csv(os.path.join(DATA_DIR,
                                           f'{self.dataset_name}.tsv'),
                              sep='\t', header=None)

        self.df.drop(columns=self.df.columns[0],
                     axis=1,
                     inplace=True)

        self.df.columns = [0, 1, 2, 3]
        df_train, df_test = np.split(self.df.sample(frac=1, random_state=42), [int(.8 * len(self.df))])

        self.train_set = Dataset(df_train)
        self.test_set = Dataset(df_test)
        self.train_dataloader = torch.utils.data.DataLoader(self.train_set, batch_size=self.batch_size, shuffle=True)
        self.test_dataloader = torch.utils.data.DataLoader(self.test_set, batch_size=self.batch_size, shuffle=True)
        """================================================"""
        use_cuda = torch.cuda.is_available()
        device = torch.device("cuda" if use_cuda else "cpu")
        self.device = device
        print(f'=========== USING {device} ===============')
        self.loss = torch.nn.BCELoss().to(self.device)
        # dim, ent_num, rel_num, resume_training=False
        self.model = TransE_Proba(dim=50, ent_num=self.train_set.ent_num,
                                  rel_num=self.train_set.rel_num, device=self.device)

        self.optimizer = torch.optim.SGD(self.model.parameters(), lr=self.learning_rate)

    def train(self):
        for epoch in range(self.epochs):
            total_train_loss = 0
            tmp = None
            t_tmp = None
            self.model = self.model.to(self.device)
            self.model.train()
            for x, y in tqdm(self.train_dataloader):
                self.optimizer.zero_grad()
                x = torch.transpose(x, 0, 1)
                pred_y = self.model(x)  # pred_y device on cuda
                tmp = pred_y
                t_tmp = y
                loss = self.loss(pred_y.unsqueeze(1), y)
                # loss = self.loss(pred_y, y)
                loss.mean().backward()
                loss = loss.data.cuda()
                self.optimizer.step()
                self.step += 1
                total_train_loss += loss.mean().item()

            if epoch % 10 == 0:
                self.save_model()
                print(tmp)

            print(f'total train loss {total_train_loss}')

    def evaluate(self):
        pass


if __name__ == '__main__':
    trainer = Trainer()
    trainer.train()
