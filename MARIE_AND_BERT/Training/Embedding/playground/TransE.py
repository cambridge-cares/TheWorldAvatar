from typing import Any

import numpy as np
import torch
import torch.nn as nn
from torch import Tensor
import torch.nn.functional as F


def _init_ent_embedding(dim=20, num=200):
    ent_embedding = nn.Embedding(embedding_dim=dim, num_embeddings=num)
    ent_embedding.weight.data.uniform_(-1, 1)
    return ent_embedding


def _init_rel_embedding(dim=20, num=21):
    rel_embedding = nn.Embedding(embedding_dim=dim, num_embeddings=num)
    rel_embedding.weight.data.uniform_(-1, 1)
    return rel_embedding


class TransE(nn.Module):

    def __init__(self):
        super(TransE, self).__init__()
        self.ent_embedding = _init_ent_embedding()
        self.rel_embedding = _init_rel_embedding()
        self.criterion = nn.MarginRankingLoss(margin=0.1, reduction='none')

    def loss(self, positive_distances, negative_distances):
        target = torch.tensor([-1], dtype=torch.long, device='cpu')
        return self.criterion(positive_distances, negative_distances, target)

    def forward(self, positive_triplets, negative_triplets):
        # max(0, - p * (x1 - x2))

        # r = non-linear(bert(q))
        # replace r to the embedding of the question, in the dimension of 50
        # score(e_h, r, e_t), where r comes from the question embedding
        # loss is still loss(dist_positive,m dist_negative), where MarginRankingLoss is the criterion

        # x1, as small as possible
        dist_positive = self.distance(positive_triplets)
        # x2, as big as possible
        dist_negative = self.distance(negative_triplets)

        return self.loss(dist_positive, dist_negative)

    def distance(self, triplet):
        head = self.ent_embedding(triplet[:, 0])
        rel = self.rel_embedding(triplet[:, 1])
        tail = self.ent_embedding(triplet[:, 2])

        norm_h_e = F.normalize(head, p=1, dim=-1)
        norm_r_e = F.normalize(rel, p=1, dim=-1)
        norm_t_e = F.normalize(tail, p=1, dim=-1)
        # print(norm_h_e)

        # if you are getting a low number, it means the triple is good, because the distance is short.
        # therefore, the goal is to minimize the number here
        # for the learning, the loss the rank_margin, max(0, -p * (x1 - x2))
        # where x1 is a positive triple and x2 is the negative tripe, p = -1 which assumes that x2 > x1
        # So, the ideal situation to minimize the loss is that x1 = 0, x2 = 1, which results in that 1 * (x1 - x2) = -1,
        # where the loss is 0. if x2 < x1, then the value will be the drift ...
        # therefore, the distance must be normalized into a [0,1] space

        return torch.norm(norm_h_e + norm_r_e - norm_t_e, p=2, dim=-1)

    def predict(self, triplets: torch.LongTensor):
        """Calculated dissimilarity score for given triplets.
        :param triplets: triplets in Bx3 shape (B - batch, 3 - head, relation and tail)
        :return: dissimilarity score for given triplets
        """
        return self._distance(triplets)