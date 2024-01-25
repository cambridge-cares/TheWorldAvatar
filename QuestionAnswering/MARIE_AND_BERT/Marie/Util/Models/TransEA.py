import os

import pandas
import torch
from torch import nn
from torch.nn.init import xavier_uniform_

from Marie.Util.Dataset.TransE_Dataset import Dataset
from Marie.Util.location import DATA_DIR
from torchmetrics import MeanSquaredLogError


class TransEA(nn.Module):
    """
    TransEA is an extension of the TransE embedding, which includes the embedding of numerical attributes
    For research purpose, this embedding will only be applied to the Wikidata dataset.
    For all triples with numerical value, TransEA will train a set of embedding a (attribute) which serves as a
    projection matrix for roughly estimating the numerical value (v) for a certain pair of e (entity) and a (attribute)
    e*a \approx v. This estimation is then used for filtering based on numerical condiations e.g. larger than 5, around
    10, similar to benzene on molar mass ...
    """

    def __init__(self, dim, ent_num, rel_num, resume_training=False, device='cuda', dataset_path="ontospecies",
                 alpha=0.1):
        super(TransEA, self).__init__()
        self.alpha = alpha
        self.dim = dim
        self.ent_num = ent_num
        self.rel_num = rel_num
        self.dataset_path = dataset_path

        if resume_training:
            print('Loading pretrained embeddings')
            self.ent_embedding = self.load_ent_embedding()
            self.rel_embedding = self.load_rel_embedding()
            self.attr_embedding = self.load_attr_embedding()
            self.bias = self.load_bias_embedding()
        else:
            self.ent_embedding = self._init_ent_embedding()
            self.rel_embedding = self._init_rel_embedding()
            self.attr_embedding = self._init_attr_embedding()
            self.bias = nn.Embedding(embedding_dim=1, num_embeddings=self.rel_num + 1, padding_idx=self.rel_num)
        self.device = device
        self.criterion = nn.MarginRankingLoss(margin=5, reduction='none').to(device)
        self.norm = 1
        self.msle_loss = MeanSquaredLogError().to(self.device)
        # A = torch.empty(self.dim, device='cpu')
        # self.bias = nn.Parameter(A)

        # self.bias.weight.data.uniform_(-1, 1)

    def load_attr_embedding(self):
        print(f"loading embedding from {os.path.join(DATA_DIR, self.dataset_path, 'attr_embedding.tsv')}")
        tsv_file_ent = pandas.read_csv(os.path.join(DATA_DIR, self.dataset_path, 'attr_embedding.tsv'), sep='\t',
                                       header=None)
        pretrained_attr_embedding = torch.FloatTensor(tsv_file_ent.values)
        self.attr_embedding = nn.Embedding.from_pretrained(pretrained_attr_embedding).requires_grad_(True)
        return self.attr_embedding

    def load_bias_embedding(self):
        print(f"loading embedding from {os.path.join(DATA_DIR, self.dataset_path, 'bias_embedding.tsv')}")
        tsv_file_bias = pandas.read_csv(os.path.join(DATA_DIR, self.dataset_path, 'bias_embedding.tsv'), sep='\t',
                                        header=None)
        pretrained_bias_embedding = torch.FloatTensor(tsv_file_bias.values)
        self.bias_embedding = nn.Embedding.from_pretrained(pretrained_bias_embedding).requires_grad_(True)
        return self.bias_embedding

    def load_ent_embedding(self):
        print(f"loading embedding from {os.path.join(DATA_DIR, self.dataset_path, 'ent_embedding.tsv')}")
        tsv_file_ent = pandas.read_csv(os.path.join(DATA_DIR, self.dataset_path, 'ent_embedding.tsv'), sep='\t',
                                       header=None)
        pretrained_ent_embedding = torch.FloatTensor(tsv_file_ent.values)
        self.ent_embedding = nn.Embedding.from_pretrained(pretrained_ent_embedding).requires_grad_(True)
        return self.ent_embedding

    def load_rel_embedding(self):
        tsv_file_rel = pandas.read_csv(os.path.join(DATA_DIR, self.dataset_path, 'rel_embedding.tsv'), sep='\t',
                                       header=None)
        pretrained_rel_embedding = torch.FloatTensor(tsv_file_rel.values)
        self.rel_embedding = nn.Embedding.from_pretrained(pretrained_rel_embedding).requires_grad_(True)
        # self.rel_embedding.weight.data[:-1, :].div_(self.rel_embedding.weight.data[:-1, :].norm(p=1, dim=1, keepdim=True))
        return self.rel_embedding

    def _init_ent_embedding(self):
        ent_embedding = nn.Embedding(embedding_dim=self.dim, num_embeddings=self.ent_num + 1, padding_idx=self.ent_num)
        ent_embedding.weight.data.uniform_(-1, 1)
        # xavier_uniform_(ent_embedding.weight.data)
        return ent_embedding

    def _init_attr_embedding(self):
        attr_embedding = nn.Embedding(embedding_dim=self.dim, num_embeddings=self.rel_num + 1, padding_idx=self.rel_num)
        # attr_embedding.weight.data.uniform_(-1, 1)
        # xavier_uniform_(attr_embedding.weight.data)

        return attr_embedding

    def _init_rel_embedding(self):
        rel_embedding = nn.Embedding(embedding_dim=self.dim, num_embeddings=self.rel_num + 1, padding_idx=self.rel_num)
        rel_embedding.weight.data.uniform_(-1, 1)
        #  rel_embedding.weight.data[:-1, :].div_(rel_embedding.weight.data[:-1, :].norm(p=1, dim=1, keepdim=True))

        return rel_embedding

    def loss(self, positive_distances, negative_distances):
        target = torch.tensor([-1], dtype=torch.long, device=self.device)
        return self.criterion(positive_distances, negative_distances, target).to(self.device)

    def forward(self, positive_triplets, negative_triplets, numerical_list=None, is_numerical=False):
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

        # self.ent_embedding.weight.data[:-1, :].div_(self.ent_embedding.weight.data[:-1, :].norm(p=1, dim=1, keepdim=True))
        # self.rel_embedding.weight.data[:-1, :].div_(self.rel_embedding.weight.data[:-1, :].norm(p=1, dim=1, keepdim=True))
        #
        # self.attr_embedding.weight.data[:-1, :].div_(
        #     self.attr_embedding.weight.data[:-1, :].norm(p=1, dim=1, keepdim=True))
        dist_positive = self.distance(positive_triplets).to(self.device)
        # x2, as big as possible
        dist_negative = self.distance(negative_triplets).to(self.device)
        if is_numerical:
            return (1 - self.alpha) * torch.mean(self.loss(dist_positive, dist_negative).to(self.device) \
                                                 + self.alpha * self.numerical_loss(positive_triplets, numerical_list))
            # return self.numerical_loss(positive_triplets, numerical_list)
            # return self.numerical_loss(positive_triplets, numerical_list)
        #  return torch.mean(self.loss(dist_positive, dist_negative).to(self.device))
        else:
            # return self.loss(dist_positive, dist_negative).to(self.device)
            return torch.mean(self.loss(dist_positive, dist_negative).to(self.device))

    def numerical_loss(self, triples, numerical_list):
        # lookup the embedding of heads
        heads = self.ent_embedding(triples[0]).to(self.device)
        # lookup the embedding of attributes
        attrs = self.attr_embedding(triples[1]).to(self.device)
        bias = self.bias(triples[1]).to(self.device)
        aV = torch.sum(heads * attrs, dim=1).to(self.device)
        value = (aV + bias)[0]
        # print("value 1", value)
        value = value.to(torch.float32)

        attr_loss = self.msle_loss(value.to(self.device), numerical_list.to(torch.float32).to(self.device)).to(self.device)
        # print("value 2", value)
        # print(numerical_list)
        # target = value - numerical_list.to(self.device)
        # attr_loss = torch.std(value.to(self.device) - numerical_list.to(self.device), unbiased=False).to(self.device)
        # attr_loss = torch.nn.functional.mse_loss(value.to(self.device),
        #                                          numerical_list.to(torch.float32).to(self.device))
        # print("attr loss", attr_loss)
        # attr_loss = torch.sum(torch.abs(target.to(self.device)) / torch.abs(numerical_list.to(self.device))).to(self.device)
        # attr_loss = torch.clamp(attr_loss,  max=1000)
        # print(p)
        # print("----")
        # print(torch.abs(p - numerical_list).shape)
        return attr_loss
        # print("attr_loss", attr_loss)
        # return torch.clamp(attr_loss, max = 1000)

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
        # CID1    charge    CID3_charge
        return (head + rel - tail).norm(p=self.norm, dim=1).to(self.device)

    def predict_hrt(self, triplets: torch.LongTensor):
        """Calculated dissimilarity score for given triplets.
        :param triplets: triplets in Bx3 shape (B - batch, 3 - head, relation and tail)
        :return: dissimilarity score for given triplets
        """
        return self.distance(triplets).to(self.device)

    def predict_numeric(self, triples):
        # lookup the embedding of heads
        heads = self.ent_embedding(triples[0]).to(self.device)
        # lookup the embedding of attributes
        attrs = self.attr_embedding(triples[1]).to(self.device)
        bias = self.bias(triples[1]).to(self.device)
        aV = torch.sum(heads * attrs, dim=1).to(self.device)
        value = (aV + bias)[0].to(self.device)
        return value
