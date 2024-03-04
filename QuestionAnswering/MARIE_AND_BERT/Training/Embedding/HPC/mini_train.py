import os

import pandas
import torch
from torch import nn

from Marie.Util.Dataset.TransE_Dataset import Dataset
from Marie.Util.location import DATA_DIR
from Training.Embedding.TransE_Trainer import Trainer as TransETrainer


class TransE(nn.Module):

    def __init__(self, dim, ent_num, rel_num, resume_training=False, device='cpu'):
        super(TransE, self).__init__()
        self.dim = dim
        self.ent_num = ent_num
        self.rel_num = rel_num

        self.ent_embedding = self._init_ent_embedding()
        self.rel_embedding = self._init_rel_embedding()
        if resume_training:
            print('Loading pretrained embeddings')
            self.ent_embedding = self.load_ent_embedding()
            self.rel_embedding = self.load_rel_embedding()

        self.device = device
        self.criterion = nn.MarginRankingLoss(margin=5, reduction='none').to(device)
        self.norm = 1

    def load_ent_embedding(self):
        tsv_file_ent = pandas.read_csv(os.path.join(DATA_DIR, 'ent_embedding.tsv'), sep='\t', header=None)
        pretrained_ent_embedding = torch.FloatTensor(tsv_file_ent.values)
        self.ent_embedding = nn.Embedding.from_pretrained(pretrained_ent_embedding).requires_grad_(True)
        return self.ent_embedding

    def load_rel_embedding(self):
        tsv_file_rel = pandas.read_csv(os.path.join(DATA_DIR, 'rel_embedding.tsv'), sep='\t', header=None)
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
        #  rel_embedding.weight.data[:-1, :].div_(rel_embedding.weight.data[:-1, :].norm(p=1, dim=1, keepdim=True))

        return rel_embedding

    def loss(self, positive_distances, negative_distances):
        target = torch.tensor([-1], dtype=torch.long, device=self.device)
        return self.criterion(positive_distances, negative_distances, target).to(self.device)

    def forward(self, positive_triplets, negative_triplets):
        """
        max(0, - p * (x1 - x2))

        r = non-linear(bert(q))
        replace r to the embedding of the question, in the dimension of 50
        score(e_h, r, e_t), where r comes from the question embedding
        loss is still loss(dist_positive,m dist_negative), where MarginRankingLoss is the criterion
        self.ent_embedding.weight.data[:-1, :].div_(
           self.ent_embedding.weight.data[:-1, :].norm(p=2, dim=1, keepdim=True))

        x1, as small as possible
        """

        self.ent_embedding.weight.data[:-1, :].div_(
            self.ent_embedding.weight.data[:-1, :].norm(p=2, dim=1, keepdim=True))

        dist_positive = self.distance(positive_triplets).to(self.device)
        # x2, as big as possible
        dist_negative = self.distance(negative_triplets).to(self.device)
        return self.loss(dist_positive, dist_negative).to(self.device)

    def distance(self, triplet):
        head = self.ent_embedding(triplet[0]).to(self.device)
        rel = self.rel_embedding(triplet[1]).to(self.device)
        tail = self.ent_embedding(triplet[2]).to(self.device)

        '''
        if you are getting a low number, it means the triple is good, because the distance is short.
        therefore, the goal is to minimize the number here
        for the learning, the loss the rank_margin, max(0, -p * (x1 - x2))
        where x1 is a positive triple and x2 is the negative tripe, p = -1 which assumes that x2 > x1
        So, the ideal situation to minimize the loss is that x1 = 0, x2 = 1, which results in that 1 * (x1 - x2) = -1,
        where the loss is 0. if x2 < x1, then the value will be the drift ...
        therefore, the distance must be normalized into a [0,1] space
        '''

        return (head + rel - tail).norm(p=self.norm, dim=1).to(self.device)

    def predict(self, triplets: torch.LongTensor):
        """Calculated dissimilarity score for given triplets.
        :param triplets: triplets in Bx3 shape (B - batch, 3 - head, relation and tail)
        :return: dissimilarity score for given triplets
        """

        return self.distance(triplets).to(self.device)


if __name__ == '__main__':
    train_triplets = [line.split('\t') for line in
                      open(os.path.join('pubchemini-train.txt')).read().splitlines()]

    test_triplets = [line.split('\t') for line in
                     open(os.path.join('pubchemini-test.txt')).read().splitlines()]

    train_set = Dataset(train_triplets)
    test_set = Dataset(test_triplets)

    e_num = train_set.ent_num
    r_num = train_set.rel_num
    model = TransE(dim=50, ent_num=e_num, rel_num=r_num)
    my_transe_trainer = TransETrainer(dataset_name="pubchemini", load_pretrained_embeddings=False, dim=20,
                                      batch_size=64)
    my_transe_trainer.train()
